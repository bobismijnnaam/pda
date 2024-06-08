package pda

import scala.collection.mutable.ArrayBuffer

import Pda._

sealed trait Ebnf {
  def +(right: Ebnf): Ebnf = Seqn(this, right)
  def opt: Ebnf = Opt(this)
  def star: Ebnf = Star(this)
  def plus: Ebnf = Plus(this)
  def |(right: Ebnf): Ebnf = Alts(this, right)
}

case class Alts(langs: Ebnf*) extends Ebnf
case class Seqn(langs: Ebnf*) extends Ebnf
case class Opt(lang: Ebnf) extends Ebnf
case class Star(lang: Ebnf) extends Ebnf
case class Plus(lang: Ebnf) extends Ebnf
case class t(text: String) extends Ebnf

case class nt(name: String) extends Ebnf {
  def ~>(production: Ebnf): Production = Production(this, production)
}

case class Production(nonTerminal: nt, production: Ebnf)

sealed trait CfgInfo extends Info {
  override def toString: String = this match {
    case ChooseAlt(i) => s"alt$i"
    case OptNone => "none"
    case OptSome => "some"
    case StarDone => "*."
    case StarMore => "*"
    case PlusDone => "+."
    case PlusMore => "+"
    case MatchToken(terminal) => s"'${terminal.text}'"
    case PushNt(nonTerminal) => s"↓${nonTerminal.name}"
    case PopNt(nonTerminal) => s"↑${nonTerminal.name}"
  }
}
case class ChooseAlt(i: Int) extends CfgInfo
object OptNone extends CfgInfo
object OptSome extends CfgInfo
object StarDone extends CfgInfo
object StarMore extends CfgInfo
object PlusDone extends CfgInfo
object PlusMore extends CfgInfo
case class MatchToken(terminal: t) extends CfgInfo
case class PushNt(nonTerminal: nt) extends CfgInfo
case class PopNt(nonTerminal: nt) extends CfgInfo

case class Cfg(productions: Production*) {
  val productionMap: Map[nt, Ebnf] = productions.map(p => p.nonTerminal -> p.production).toMap

  require(productionMap.size == productions.size)

  productionMap.values.foreach(check)

  private def check(ebnf: Ebnf): Unit =
    ebnf match {
      case Alts(langs@_*) => langs.foreach(check)
      case Seqn(langs@_*) => langs.foreach(check)
      case Opt(lang) => check(lang)
      case Star(lang) => check(lang)
      case Plus(lang) => check(lang)
      case t(_) =>
      case nonTerminal: nt => require(productionMap.contains(nonTerminal))
    }

  def asPda(start: nt): Pda = {
    require(productionMap.contains(start))

    val states = ArrayBuffer[State]()
    val stateNames = new Names

    def mkState(name: String): State = {
      val state = State(stateNames(name))
      states += state
      state
    }

    val transitions = ArrayBuffer[Transition]()
    val startState = productionMap.keys.map(nonTerminal => nonTerminal -> mkState(s"${nonTerminal.name}:start")).toMap
    val endState = productionMap.keys.map(nonTerminal => nonTerminal -> mkState(s"${nonTerminal.name}:end")).toMap

    val symbNames = new Names

    def mkSymb(name: String): Symbol = Symbol(symbNames(name))

    def connectEbnf(start: State, end: State, context: String, ebnf: Ebnf): Unit =
      ebnf match {
        case Alts(langs @ _*) =>
          langs.zipWithIndex.foreach { case (lang, idx) =>
            val altStart = mkState(context)
            transitions += Transition(start, TransLabel.epsilon, altStart)(ChooseAlt(idx))
            connectEbnf(altStart, end, context, lang)
          }
        case Seqn(langs @ _*) =>
          val intermediates = langs.indices.tail.map(i => mkState(context))
          langs.zip(start +: intermediates).zip(intermediates :+ end).zipWithIndex.foreach {
            case (((lang, start), end), idx) =>
              connectEbnf(start, end, context, lang)
          }
        case Opt(lang) =>
          transitions += Transition(start, TransLabel.epsilon, end)(OptNone)
          val chooseSome = mkState(context)
          transitions += Transition(start, TransLabel.epsilon, chooseSome)(OptSome)
          connectEbnf(chooseSome, end, context, lang)
        case Star(lang) =>
          transitions += Transition(start, TransLabel.epsilon, end)(StarDone)
          val chooseMore = mkState(context)
          transitions += Transition(start, TransLabel.epsilon, chooseMore)(StarMore)
          connectEbnf(chooseMore, start /* ! */, context, lang)
        case Plus(lang) =>
          val decisionState = mkState(context)
          transitions += Transition(decisionState, TransLabel.epsilon, start)(PlusMore)
          transitions += Transition(decisionState, TransLabel.epsilon, end)(PlusDone)
          connectEbnf(start, decisionState, context, lang)
        case terminal @ t(text) =>
          val intermediates = text.indices.tail.map(i => mkState(context))
          text.zip(start +: intermediates).zip(intermediates :+ end).zipWithIndex.foreach {
            case (((c, start), end), idx) =>
              val infos: Seq[Info] = if(idx == 0) Seq(MatchToken(terminal)) else Nil
              transitions += Transition(start, TransLabel.character(Character(c)), end)(infos: _*)
          }
        case nonTerminal @ nt(name) =>
          val symbol = mkSymb(s"$context:$name")
          transitions += Transition(start, TransLabel.push(symbol), startState(nonTerminal))(PushNt(nonTerminal))
          transitions += Transition(endState(nonTerminal), TransLabel.pop(symbol), end)(PopNt(nonTerminal))
      }

    for((nonTerminal, lang) <- productionMap) {
      connectEbnf(startState(nonTerminal), endState(nonTerminal), nonTerminal.name, lang)
    }

    Pda(states.toSeq, transitions.toSeq, startState(start), endState(start))
  }
}