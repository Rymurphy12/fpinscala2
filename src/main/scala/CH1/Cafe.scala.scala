package CH1


class Cafe{
    def buyCoffee(cc: CreditCard): Coffee = {
        val cup = Coffee()
        cc.charge(cup.price)
        cup
    }
        
}
        

class Coffee {
    val price = 2.0
}

class CreditCard{
    def charge(price: Double): Unit = {
        println("charging " + price)
    }
}