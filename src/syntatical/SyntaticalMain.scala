package syntatical

import java.io.{Reader, PushbackInputStream, InputStreamReader, PushbackReader}

import lexical.{TokenType, Lexer}

import scala.io.Source

object SyntaticalMain {

  def main(args:Array[String]){
    //lexer.setReader(Source.fromFile("resources/t1.txt").reader())
    val lexer = new Lexer()
    lexer.setReader(Source.stdin.reader())
    val syntatical = new Syntatical(lexer)
    while(true){
      val result = syntatical.procExpr()
      println(": "+result.value())
    }
  }
}
