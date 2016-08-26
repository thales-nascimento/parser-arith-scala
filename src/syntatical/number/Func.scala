package syntatical.number

import syntatical.Syntatical


class FuncData(args:List[NumValue], exp:NumValue){
  val expr = exp
  def apply(vals:List[NumValue]): Unit ={
    val it = vals.iterator
    args.foreach(a => {
      a.asInstanceOf[Var].reassign(it.next().value())
    })
  }
}

class Func( data:FuncData) extends NumValue{
  override def value(): Double = data.expr.value()


}

object Func{
  def apply(context:Syntatical, name:String, args:List[NumValue]): Func = {
    val opt = context.funcTable.get(name)
    if (opt.isEmpty) {
      val data = new FuncData(null, new Constant(0));
      context.funcTable.put(name, data)
      new Func(data)
    } else {
      val data:FuncData = opt.get
      data.apply(args)
      new Func(data)
    }
  }

  def apply(context:Syntatical, name:String, args:List[NumValue], expr:NumValue): Func = {
    val data = new FuncData(args, expr)
    context.funcTable.put(name, data)
    new Func(data)
  }
}