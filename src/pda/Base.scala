package pda

sealed trait MatchSymbol
case class Symbol(text: String) extends MatchSymbol
object AnySymbol extends MatchSymbol
object StartSymbol extends MatchSymbol

sealed trait MatchCharacter
case class Character(c: Char) extends MatchCharacter
object Epsilon extends MatchCharacter

case class State(name: String)
