package lexical

trait Lexerable {
  def nextToken():Lexeme
  def getLine():Int
}
