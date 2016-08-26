package syntatical.number

import lexical.TokenType

class BinaryMulOp(op:TokenType, left:NumValue, right:NumValue) extends NumValue{
  override def value(): Double ={
    if(op == TokenType.DIV) {
      left.value() / right.value();
    } else {
      left.value() * right.value();
    }
  }

}
