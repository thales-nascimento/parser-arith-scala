package lexical

import java.io.PushbackReader
import java.io.Reader

class Lexer() extends Lexerable{
  var input:PushbackReader=null
  def setReader(reader:Reader): Unit ={
    input = new PushbackReader(reader)
  }
  private object line{
    private var line = 1
    def get():Int = line
    def increment() {line += 1}
  }

  def getLine:Int = line.get()

  def isDigit(c:Int):Boolean = {
    c >= '0' && c <= '9'
  }

  def compare(c:Int, s:String): Boolean = {
    s.contains(c.asInstanceOf[Char])
  }

  def isWhiteSpace(c:Int): Boolean ={
    compare(c, "\n\t ")
  }

  def nextToken(): Lexeme = {
    val lex = new Lexeme()
    var state:Int = 1

    while(state != 15){
      val c = input.read()
      state match {
        case 1 => {
          if(c == -1 || c == 255){
            lex.tokenType = TokenType.EOF
            state = 15
          } else if(isWhiteSpace(c)){
            if(c == '\n'){
              lex.tokenType = TokenType.ENDL
              state = 15
            }
          } else if(c == '#'){
            line.increment()
            state = 2
          }else if( c.toChar.isDigit){
            lex.tokenType = TokenType.NUMBER
            lex.cat(c)
            state = 3

          } else if(c.toChar.isLetter){
            lex.cat(c)
            lex.tokenType = TokenType.VAL;
            state = 9

          } else if(c == '.'){
            lex.cat(c)
            lex.tokenType = TokenType.NUMBER
            state = 4

          } else if(c == ','){
            lex.cat(c)
            lex.tokenType = TokenType.COMMA
            state = 15

          } else if(c == ')'){
            lex.cat(c)
            lex.tokenType = TokenType.CLOSE_PAR
            state = 15

          } else if(c =='('){
            lex.cat(c)
            lex.tokenType = TokenType.OPEN_PAR
            state = 15

          } else if(c == '+'){
            lex.cat(c)
            lex.tokenType = TokenType.PLUS
            state = 15

          } else if(c == '-'){
            lex.cat(c)
            lex.tokenType = TokenType.MINUS
            state = 15

          } else if(c == '/') {
            lex.cat(c)
            lex.tokenType = TokenType.DIV
            state = 15

          } else if(c == '=') {
            lex.cat(c)
            lex.tokenType = TokenType.ASSIGN
            state = 15

          } else if(c == '*') {
            lex.cat(c)
            state = 7

          } else {
            lex.cat(c)
            lex.tokenType = TokenType.INVALID_TOKEN
            state = 15
          }
        }
        case 2 => {
          if(c == '\n'){
            state = 1
          }
        }
        case 3 => {
          if (c == '.'){
            lex.cat(c)
            state = 4
          } else if (compare(c, "eE")){
            lex.cat(c)
            state = 5
          } else if(isDigit(c)) {
            lex.cat(c);
          } else {
            input.unread(c)
            state = 15
          }
        }
        case 4 => {
          if(isDigit(c)){
            lex.cat(c)
            state = 8
          } else {
            lex.cat(c)
            lex.tokenType = TokenType.INVALID_TOKEN
            state = 15
          }
        }
        case 8 => {
          if(isDigit(c)){
            lex.cat(c)
          } else if(compare(c, "eE")) {
            lex.cat(c)
            state = 5
          } else {
            input.unread(c)
            state = 15
          }
        }
        case 5 => {
          if(isDigit(c)){
            lex.cat(c)
            state = 6
          }else if(c == '+'){
            lex.cat(c)
            state = 6
          } else if(c == '-'){
            lex.cat(c)
            state = 6
          } else {
            lex.cat(c)
            lex.tokenType = TokenType.INVALID_TOKEN
            state = 15
          }
        }
        case 6 => {
          if(isDigit(c)){
            lex.cat(c)
          } else {
            input.unread(c)
            state = 15
          }
        }
        case 7 => {
          if(c == '*'){
            lex.cat(c)
            lex.tokenType = TokenType.POW
            state = 15
          } else {
            input.unread(c)
            lex.tokenType = TokenType.MUL
            state = 15
          }
        }
        case 9 => {
          if(c.toChar.isLetter){
            lex.cat(c)
          } else {
            input.unread(c)
            state = 15
          }
        }
      }
    }

    if(lex.tokenType == TokenType.INVALID_TOKEN){
      throw new Exception("linha "+line.get()+": token invalido: "+lex.token)
    }
    lex
  }
}
