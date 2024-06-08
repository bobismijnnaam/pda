package pda

import java.nio.file.{Files, Paths}
import scala.util.Using
import Pda._

import scala.collection.mutable

object Pda {
  object TransLabel {
    def epsilon: TransLabel = peek(AnySymbol)

    def character(m: MatchCharacter): TransLabel =
      TransLabel(m, AnySymbol, pop = false, push = Nil)

    def pop(sym: MatchSymbol): TransLabel =
      TransLabel(Epsilon, sym, pop = true, push = Nil)

    def peek(sym: MatchSymbol): TransLabel =
      TransLabel(Epsilon, sym, pop = false, push = Nil)

    def push(symbols: Symbol*): TransLabel =
      TransLabel(Epsilon, AnySymbol, pop = false, push = symbols)
  }

  case class TransLabel(character: MatchCharacter, symbol: MatchSymbol, pop: Boolean, push: Seq[Symbol]) {
    require(symbol != StartSymbol || !pop)

    def charString: String = character match {
      case Character(c) => c.toString
      case Epsilon => "λ"
    }

    def symbolString: String = symbol match {
      case Symbol(text) => text
      case AnySymbol if !pop => "λ"
      case AnySymbol => "*"
      case StartSymbol => "Z"
    }

    def repushString: String =
      if(pop) ""
      else symbol match {
        case Symbol(text) => text + " "
        case AnySymbol => ""
        case StartSymbol => "" // stays implicitly I guess?
      }

    def pushTailString: String = push.map(_.text).mkString(" ")

    def pushString = {
      val initial = repushString + pushTailString
      if(initial.isBlank) "λ"
      else initial
    }

    override def toString: String =
      s"$charString, $symbolString ; $pushString"
  }

  trait Info
  case class Transition(left: State, cond: TransLabel, right: State)(val info: Info*) {
    def label: String = {
      val infos = info.mkString(" [", " ", "]")
      s"$cond$infos"
    }

    override def toString: String = s"$left --[$label]--> $right"
  }

  def product(P: Pda, D: Nfa): Pda = {
    val visited = mutable.LinkedHashSet[State]()
    val states = mutable.LinkedHashMap[(State, State), State]()
    val edges = mutable.LinkedHashMap[(Pda.Transition, Nfa.Transition), Pda.Transition]()

    def state(p: State, d: State): State =
      states.getOrElseUpdate((p, d), {
        State(s"${p.name}&${d.name}")
      })

    def outgoing(p: State, d: State): Seq[(Pda.Transition, Nfa.Transition)] = {
      P.outgoing(p)
        .flatMap { t_P => D.outgoing(d).map { t_D => (t_P, t_D) }}
        .filter { case (t_P, t_D) => t_P.cond.character == t_D.c }
    }

    def dfs(p: State, d: State): Unit = {
      val left = state(p, d)
      if (visited(left)) return
      visited.add(left)
      for ((tp, td) <- outgoing(p, d)) {
        val transition = Pda.Transition(left, tp.cond, state(tp.right, td.to))()
        assert(!edges.contains((tp, td)))
        edges((tp, td)) = transition
        dfs(tp.right, td.to)
      }
    }

    dfs(P.s0, D.s0)

    Pda(states.values.toSeq, edges.values.toSeq, state(P.s0, D.s0), state(P.accept, D.accept))
  }
}

case class Pda(states: Seq[State], transs: Seq[Transition], s0: State, accept: State) {
//  require(states.contains(s0))
//  require(states.contains(accept))
//  require(transs.forall(t => states.contains(t.left) && states.contains(t.right)))

  override def toString = transs.mkString("\n")

  def toDot(): Unit =
    Using.resource(Files.newBufferedWriter(Paths.get(s"${s0.name}.dot"))) { writer =>
      def quote(s: String): String = s"\"${s.replace("\n", "\\n")}\""

      writer.write("digraph {\n")

      for(state <- states) {
        if(s0 == state) {
          writer.write(s"START [label=${quote("")} shape=none]")
          writer.write(s"${quote(state.name)}\n")
          writer.write(s"START -> ${quote(state.name)}")
        } else if(accept == state) {
          writer.write(s"${quote(state.name)} [shape=doublecircle]\n")
        } else {
          writer.write(s"${quote(state.name)}\n")
        }
      }

      val transitionsByPairs = transs.groupBy(t => (t.left, t.right))

      for(((left, right), transs) <- transitionsByPairs) {
        writer.write(s"${quote(left.name)} -> ${quote(right.name)} [label=${quote(transs.map(_.label).mkString("\n"))}]")
      }

      writer.write("}\n")
    }

  def outgoing(s: State): Seq[Transition] = transs.filter(_.left == s)
}

