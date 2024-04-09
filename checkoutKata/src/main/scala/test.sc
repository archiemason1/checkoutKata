case class Checkout(pricingRules: Map[Char, (Int, Int, Int)]) {
  def printPricingRules(element: Char) = pricingRules(element)
}
val price = new Checkout(Map('a' -> (50, 3, 130), 'b' -> (30, 2, 45), 'c' -> (20, 1, 20), 'd' -> (15, 1, 15)))
println(price)
price.printPricingRules('a')._1