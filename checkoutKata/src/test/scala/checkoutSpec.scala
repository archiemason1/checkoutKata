import org.scalatest.funsuite.AnyFunSuite
import checkoutKataCode.Checkout
import checkoutKataCode.PricingRule

class CheckoutSpec extends AnyFunSuite {
  test("total price calculation") {
    val pricingRules = Map(
      'a' -> PricingRule(50, 3, 130),
      'b' -> PricingRule(30, 2, 45),
      'c' -> PricingRule(20, 1, 20),
      'd' -> PricingRule(15, 1, 15)
    )
    val checkout = new Checkout(pricingRules)

    val items = List('a', 'a', 'a', 'b', 'b', 'c', 'd', 'x')
    val totalPrice = checkout.calculateTotalPrice(items)

    assert(totalPrice == 130 + 45 + 20 + 15)
  }
}


