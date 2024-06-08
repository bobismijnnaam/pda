package pda

import Nfa._

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.IteratorHasAsScala

object Nfa {
  case class Transition(from: State, c: MatchCharacter, to: State)

  def stringAutomaton(input: String): Nfa = {
    val states = ArrayBuffer[State]()
    val s0 = State("s0")
    states.addOne(s0)
    val transitions = ArrayBuffer[Transition]()
    val last = input.chars().iterator().asScala.zipWithIndex.foldLeft(s0) {
      case (prev, (c, i)) =>
        val next = State(s"s${i+1}")
        states.addOne(next)
        transitions.addOne(Transition(prev, Character(c.toChar), next))
        next
    }

    val epsilonTransitions = states.map { s =>
      Transition(s, Epsilon, s)
    }

    Nfa(states.toSeq, transitions.toSeq ++ epsilonTransitions.toSeq, s0, last)
  }
}

case class Nfa(states: Seq[State], transitions: Seq[Transition], s0: State, accept: State) {
  require(states.contains(s0) && states.contains(accept))
  require(transitions.forall(t => states.contains(t.from) && states.contains(t.to)))

  def toPda(): Pda = {
    import Pda._
    Pda(
      states,
      transitions.map { t =>
        Transition(
          t.from,
          TransLabel.character(t.c),
          t.to
        )()
      },
      s0,
      accept
    )
  }

  def outgoing(s: State): Seq[Transition] = transitions.filter(_.from == s)
}
