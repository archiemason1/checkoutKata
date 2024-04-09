import org.scalatest.funsuite.AnyFunSuite
import checkoutKataCode._
class CheckoutSpec extends AnyFunSuite {
  // these vals could be reused for other potential tests
  // so may as well put them in scope to all tests, not defined inside one.
  val pricingRules = Map(
    'a' -> PricingRule(50, 3, 130),
    'b' -> PricingRule(30, 2, 45),
    'c' -> PricingRule(20, 1, 20),
    'd' -> PricingRule(15, 1, 15)
  )
  val checkout = new Checkout(pricingRules)
  val items = List('a', 'a', 'a', 'b', 'b', 'c', 'd', 'x')


  // you could now write a test, checking that the countItemsToMap method is doing what it should
  // again this is pretty trivial stuff, but good to get in the habit of having methods that are easy to test
  // (they only do one thing)
  test("total price calculation") {
    val countItems: Map[Char, Int] = checkout.countItemsToMap(items)
    val totalPrice: Int = checkout.calculateTotalPrice(countItems)

    assert(totalPrice == 130 + 45 + 20 + 15)
  }
}
