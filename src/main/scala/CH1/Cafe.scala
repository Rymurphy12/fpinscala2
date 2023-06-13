package CH1


class Cafe{
    def buyCoffee(cc: CreditCard, p: Payments): Coffee = {
        val cup = Coffee()
        p.charge(cc, cup.price)
        cup
    }
        
}

trait Payments{
    def charge(cc: CreditCard, price: Double): Unit
}

class SimulatedPayments extends Payments{
    def charge(cc: CreditCard, price: Double): Unit = {
        println("charging " + price + " to " + cc)
    }
}        

class Coffee {
    val price = 2.0
}

class CreditCard