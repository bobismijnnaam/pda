package pda

import scala.collection.mutable.ArrayBuffer

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

case class ChooseAlt(i: Int) extends Info
object OptNone extends Info
object OptSome extends Info
object StarDone extends Info
object StarMore extends Info
object PlusDone extends Info
object PlusMore extends Info
case class MatchToken(terminal: t) extends Info
case class PushNt(nonTerminal: nt) extends Info
case class PopNt(nonTerminal: nt) extends Info

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

    def mkState(name: String): State = {
      val state = State(name)
      states += state
      state
    }

    val transitions = ArrayBuffer[Transition]()
    val startState = productionMap.keys.map(nonTerminal => nonTerminal -> mkState(s"${nonTerminal.name}:start")).toMap
    val endState = productionMap.keys.map(nonTerminal => nonTerminal -> mkState(s"${nonTerminal.name}:end")).toMap

    def connectEbnf(start: State, end: State, trail: String, ebnf: Ebnf): Unit =
      ebnf match {
        case Alts(langs @ _*) =>
          langs.zipWithIndex.foreach { case (lang, idx) =>
            val altTrail = s"$trail:alt$idx"
            val altStart = mkState(s"$altTrail:start")
            transitions += Transition(start, TransLabel.epsilon, altStart)(ChooseAlt(idx))
            connectEbnf(altStart, end, altTrail, lang)
          }
        case Seqn(langs @ _*) =>
          val intermediates = langs.indices.tail.map(i => mkState(s"$trail:seqn$i"))
          langs.zip(start +: intermediates).zip(intermediates :+ end).zipWithIndex.foreach {
            case (((lang, start), end), idx) =>
              val seqnTrail = s"$trail:seqn$idx"
              connectEbnf(start, end, seqnTrail, lang)
          }
        case Opt(lang) =>
          transitions += Transition(start, TransLabel.epsilon, end)(OptNone)
          val chooseSome = mkState(s"$trail:some")
          transitions += Transition(start, TransLabel.epsilon, chooseSome)(OptSome)
          connectEbnf(chooseSome, end, s"$trail:some", lang)
        case Star(lang) =>
          transitions += Transition(start, TransLabel.epsilon, end)(StarDone)
          val chooseMore = mkState(s"$trail:more")
          transitions += Transition(start, TransLabel.epsilon, chooseMore)(StarMore)
          connectEbnf(chooseMore, start /* ! */, s"$trail:more", lang)
        case Plus(lang) =>
          val decisionState = mkState(s"$trail:plus")
          transitions += Transition(decisionState, TransLabel.epsilon, start)(PlusMore)
          transitions += Transition(decisionState, TransLabel.epsilon, end)(PlusDone)
          connectEbnf(start, decisionState, s"$trail:plus", lang)
        case terminal @ t(text) =>
          val intermediates = text.indices.tail.map(i => mkState(s"$trail:char$i"))
          text.zip(start +: intermediates).zip(intermediates :+ end).zipWithIndex.foreach {
            case (((c, start), end), idx) =>
              val infos: Seq[Info] = if(idx == 0) Seq(MatchToken(terminal)) else Nil
              transitions += Transition(start, TransLabel.character(Character(c)), end)(infos: _*)
          }
        case nonTerminal @ nt(name) =>
          val ntTrail = s"$trail:$name"
          transitions += Transition(start, TransLabel.push(Symbol(ntTrail)), startState(nonTerminal))(PushNt(nonTerminal))
          transitions += Transition(endState(nonTerminal), TransLabel.pop(Symbol(ntTrail)), end)(PopNt(nonTerminal))
      }

    for((nonTerminal, lang) <- productionMap) {
      connectEbnf(startState(nonTerminal), endState(nonTerminal), nonTerminal.name, lang)
    }

    Pda(states.toSeq, transitions.toSeq, startState(start), endState(start))
  }
}