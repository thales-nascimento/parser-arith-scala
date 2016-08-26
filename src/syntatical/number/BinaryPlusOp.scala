package syntatical.number

import lexical.TokenType

class BinaryPlusOp(op:TokenType, left:NumValue, right:NumValue) extends NumValue{
  override def value(): Double =
    if(op == TokenType.PLUS) {
      left.value() + right.value()
    } else {
      left.value() - right.value()
    }

}
