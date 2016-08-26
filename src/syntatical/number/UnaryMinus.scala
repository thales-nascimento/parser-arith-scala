package syntatical.number

import lexical.TokenType


class UnaryMinus(operand:NumValue) extends NumValue{
  override def value():Double = -operand.value()
}
