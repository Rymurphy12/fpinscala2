package fpinscala2.CH1


class Cafe{
    def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
        val cup = Coffee()
        (cup, Charge(cc, cup.price))
    }

    def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
        val purchases = List.fill(n)(buyCoffee(cc))
        val (coffees, charges) = purchases.unzip
        val reduced = charges.reduce((c1, c2) => c1.combine(c2))
        (coffees, reduced)
    }
        
}

case class Charge(cc: CreditCard, amount: Double){
    def combine(other: Charge): Charge = {
        if cc == other.cc then
            Charge(cc, amount + other.amount)
        else
            throw Exception("Can't combine charges with different cards")
    }
}
   
class Coffee {
    val price = 2.0
}

class CreditCard