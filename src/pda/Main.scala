package pda

import java.nio.file.{Files, Paths}
import scala.util.Using

object Main {
  def main(args: Array[String]): Unit = {
    val expr = nt("expr")
    val num = nt("num")

    val cfg = Cfg(
      expr ~> (expr + t("+") + expr | expr + t("*") + expr | num),
      num ~> (t("0") | t("1")),
    )

    val N0 = nt("Zero")
    val N1 = nt("One")
    val cfg2 = Cfg(
      N0 ~> (t("0") + N0 | N1),
      N1 ~> (t("1") + N1 | t("EOF"))
    )

    val dfa = Nfa.stringAutomaton("000111")

    Pda.product(cfg2.asPda(N0), dfa).toDot()
//    Dfa.stringAutomaton("Bob").toPda().toDot()
  }
}