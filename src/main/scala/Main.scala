import CH1._
@main def hello(): Unit = {
  val cc = CreditCard()
  val p = SimulatedPayments()
  val cafe = Cafe()
  val cup = cafe.buyCoffee(cc, p)

}
