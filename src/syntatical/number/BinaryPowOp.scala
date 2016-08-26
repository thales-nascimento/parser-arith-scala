package syntatical.number

class BinaryPowOp(power:NumValue, base:NumValue) extends NumValue{
  override def value(): Double = Math.pow(base.value(), power.value())
}
