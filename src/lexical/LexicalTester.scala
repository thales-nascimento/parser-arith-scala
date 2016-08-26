package lexical

import java.io.PushbackInputStream

import scala.io.Source

object LexicalTester {
  def main(args:Array[String]){
    val lexer = new Lexer()
    lexer.setReader(Source.fromFile("resources/t1.txt").reader())
    var lexeme= new Lexeme()

    while(lexeme.tokenType != TokenType.INVALID_TOKEN && lexeme.tokenType != TokenType.EOF && lexeme.tokenType != TokenType.UNEXPECTED_EOF){
      lexeme = lexer.nextToken()
      println(lexeme)
    }
    if(lexeme.tokenType == TokenType.UNEXPECTED_EOF){
      println("Unexpected end of file")
    } else if(lexeme.tokenType == TokenType.INVALID_TOKEN){
      println("Invalid token")
    }
    println("analise lexica terminou")
  }
}
