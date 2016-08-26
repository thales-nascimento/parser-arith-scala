package lexical

class Lexeme {
  var token:String = ""
  var tokenType:TokenType = TokenType.EMPTY

  def cat(c:Int){
    token = token + c.toChar
  }

  override def toString() = {
    token + ":" + tokenType.toString
  }
}
