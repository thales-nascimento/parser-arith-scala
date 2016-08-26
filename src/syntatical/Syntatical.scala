package syntatical

import lexical.{Lexerable, TokenType}
import syntatical.number._

import scala.collection.mutable

class Syntatical (lexical:Lexerable) {
  private var current = lexical.nextToken()

  val valTable = new mutable.HashMap[String, Double]()
  val funcTable = new mutable.HashMap[String, FuncData]()

  private def skipToken(): Unit ={
    current = lexical.nextToken()
  }

  def matchToken(tokenType: TokenType){
    if(tokenType == current.tokenType){
      current = lexical.nextToken()
    } else {
      val errorMessage:String = "token invalido na linha "+lexical.getLine+
        "\nEsperado: "+ tokenType+
        "\nRecebido: "+ current.tokenType
      throw new Exception(errorMessage)
    }
  }

  def procExpr(): NumValue = {
    while(current.tokenType == TokenType.ENDL) skipToken()
    var term = procSumTerm()
    if(current.tokenType == TokenType.PLUS || current.tokenType == TokenType.MINUS) do{
      val op = current.tokenType
      skipToken()
      term = new BinaryPlusOp(op, term, procSumTerm())
    } while (current.tokenType == TokenType.PLUS || current.tokenType == TokenType.MINUS)

    term
  }

  private def procSumTerm(): NumValue ={
    var term = procMulTerm()
    if(current.tokenType == TokenType.MUL || current.tokenType == TokenType.DIV) do {
      val op = current.tokenType
      skipToken()
      term = new BinaryMulOp(op, term, procMulTerm())
    } while(current.tokenType == TokenType.MUL || current.tokenType == TokenType.DIV)

    term
  }

  private def procMulTerm(): NumValue = {
    while(current.tokenType == TokenType.PLUS){
      skipToken()
    }
    if(current.tokenType == TokenType.MINUS){
      matchToken(TokenType.MINUS)
      new UnaryMinus(procMulTerm());

    } else {
      var right = procPowTerm()
      if(current.tokenType == TokenType.POW) do{
        matchToken(TokenType.POW)
        right = new BinaryPowOp(procMulTerm(), right)
      }while (current.tokenType == TokenType.POW)
      right
    }
  }

  private def procPowTerm(): NumValue = {
    if(current.tokenType == TokenType.VAL){
      procAssign()
    } else if(current.tokenType == TokenType.NUMBER){
      procNumber()
    }  else {
      matchToken(TokenType.OPEN_PAR)
      val expr = procExpr()
      matchToken(TokenType.CLOSE_PAR)
      expr
    }
  }

  private def procNumber(): Constant = {
    val number = current.token
    matchToken(TokenType.NUMBER)
    new Constant(number.toDouble)
  }

  private def procAssign(): NumValue ={
    val name = current.token
    matchToken(TokenType.VAL)
    if (current.tokenType == TokenType.OPEN_PAR) {
      procFunc(name)
    } else {
      procVal(name)
    }
  }

  private def procFunc(name:String): Func = {
    matchToken(TokenType.OPEN_PAR)
    var args:List[NumValue] = List()
    if(current.tokenType != TokenType.CLOSE_PAR){
      args = procExpr() :: args
      while(current.tokenType == TokenType.COMMA){
        matchToken(TokenType.COMMA)
        args = procExpr() :: args
      }
    }
    matchToken(TokenType.CLOSE_PAR)
    if(current.tokenType == TokenType.ASSIGN){
      matchToken(TokenType.ASSIGN)
      Func(this, name, args, procExpr())
    } else {
      Func(this, name, args)
    }
  }

  private def procVal(name:String): Var = {
    if (current.tokenType == TokenType.ASSIGN) {
      matchToken(TokenType.ASSIGN)
      new Var(this, name, procExpr())
    } else {
      Var(this, name)
    }
  }

}
