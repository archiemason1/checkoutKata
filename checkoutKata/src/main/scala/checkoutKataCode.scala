import checkoutKataCode.PricingRule
//Implement the code for a supermarket checkout that calculates the total price of a number of items.
// In a normal supermarket, things are identified using Stock Keeping Units, or SKUs. In our store,
// we’ll use individual letters of the alphabet (a, b, c, and so on as the SKUs). Our goods are priced individually.
// In addition, some items are multi-priced: buy n of them, and they’ll cost you y.
// For example, item ‘a’ might cost 50 pence individually, but this week we have a special offer:
// buy three ‘a’s and they’ll cost you £1.30. In fact this week’s prices are:
//  Item  Unit Price Special Price
//    a    50         3 for 130
//    b    30         2 for 45
//    c    20
//    d    15
//Our checkout accepts items in any order, so that if we scan a B, an A, and another B,
// we’ll recognise the two B’s and price them at 45 (for a total price so far of 95).
//  Extra points: Because the pricing changes frequently, we need to be able to pass in a set of pricing
//  rules each time we start handling a checkout transaction.
//  We expect to be able to run your solution as a command line application.

object checkoutKataCode {
  case class PricingRule(unitPrice: Int, amountToQualify: Int, specialOfferPrice: Int)

  class Checkout(pricingRules: Map[Char, PricingRule]) {
    def countItemsToMap(items: List[Char]): Map[Char, Int] = {
      items.foldLeft(Map[Char, Int]().withDefaultValue(0)) { (acc, item) =>
        acc.updated(item, acc(item) + 1)
      }
    }



    def calculateTotalPrice(itemsMap:  Map[Char, Int]): Int = {
      itemsMap.foldLeft(0) { (totalPrice, itemAndCount) =>
        val (item, count) = itemAndCount
        val rule: PricingRule = pricingRules.getOrElse(item, PricingRule(0, 1, 0)) // makes things a bit more readable, and following convention.
        // I'd try and split out the below vals into methods,
        // this would give you the opportunity to test each step individually,
        // which would make debugging easier, and give you better test coverage.
        // This is quite a simple program so debugging wouldn't be an issue, but it's good
        // to get into the habit for when things get more complex!
        val specialOffersCount = count / rule.amountToQualify // keeps the names the same, before there were 2 different names for the same thing.
        val remainingItemsCount = count % rule.amountToQualify
        val specialOffersPrice = specialOffersCount * rule.specialOfferPrice
        val remainingItemsPrice = remainingItemsCount * rule.unitPrice
        totalPrice + specialOffersPrice + remainingItemsPrice
      }
    }
  }
}
