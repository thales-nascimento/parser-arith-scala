package syntatical.number

import syntatical.Syntatical

class Var(context:Syntatical, name:String, const:Double) extends NumValue{
  def this(context:Syntatical, name:String, expr:NumValue) = this(context, name, expr.value())
  context.valTable.put(name, const)

  override def value(): Double = {
    context.valTable.get(name).get
  }

  def reassign(newValue:Double){
    context.valTable.put(name, newValue)
  }

  override def toString ={
    "val:"+name+":"+value()
  }
}

object Var{
  def apply(context:Syntatical, name:String): Var = {
    val opt = context.valTable.get(name)
    if (opt.isEmpty) {
      new Var(context, name, 0)
    } else {
      new Var(context, name, opt.get)
    }
  }
}
