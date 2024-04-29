import checkoutKataCode.{Checkout, PricingRule}

import scala.io.StdIn

object Main extends App { // I forgot you can do this instead of explicitly defining a "def main" I think you said this the other day too!"

//  This method is doing a looooooooot ;-D! When something starts getting big like this,
//  think about how you're going to test it, and if the test fails, will it be hard to debug? When you
// start getting a lot of vals in a method, it's a sign theres probably room for some method extraction.
// If all these vals were methods, you could write tests for them all, and then one for the "whole" extractPricingRules method.
// then if extractPricingRules fails, you'll probably have another mini method test failing too.
  def extractPricingRules(args: Array[String]): Map[Char, PricingRule] = {
    if (args.length < 1) {
      println("No pricing rules provided. Using default rules.")
      return defaultPricingRules
    }

    val rulesStr = args(0).split(",")

    val pricingRules = rulesStr.flatMap { ruleStr =>
      val ruleParams = ruleStr.trim.split("\\s+")
      if (ruleParams.length != 4) {
        /*
        println(
          s"""Hey
             |this
             |is how you do extra lines without \"+\"
             |""".stripMargin)
         */
        println(s"Illegal pricing rule format: $ruleStr. Skipping. Try inputting the" +
          s"pricing rules in the format \"a 30 2 55, b...\" where a -> item, 30 -> single price" +
          s"2 -> amount to qualify special price, 55 -> special price")
        None
      } else {
        try {
          val item = ruleParams(0).charAt(0); println(item)                                        /* <-| */
          val unitPrice = ruleParams(1).toInt                                       /* <-| */
          val amountToQualify = ruleParams(2).toInt                                 /* <-| */
          val specialOfferPrice = ruleParams(3).toInt                               /* <-| */
          Some(item -> PricingRule(unitPrice, amountToQualify, specialOfferPrice))  /* <-|  this could all be extracted into a method, having ruleParams as the parameter,
          and tested. BTW I think having the 4 vals here inside the extracted method would be ok. If they were any more complicated, I'd say extreact them into their own
           methods too.*/
        } catch {
          case _: NumberFormatException =>
            println(s"Illegal pricing rule format: $ruleStr. Skipping. Try inputting the" +
              s"pricing rules in the format \"a 30 2 55, b...\" where a -> item, 30 -> single price" +
              s"2 -> amount to qualify special price, 55 -> special price") // a bit nit picky, but you've written out the same string twice, could this be improved?
            None
        }
      }
    }.toMap

    if (pricingRules.isEmpty) {
      println("No valid pricing rules provided. Using default rules.")
      defaultPricingRules
    } else {
      pricingRules
    }
  }

  val defaultPricingRules = Map(
    'a' -> PricingRule(50, 3, 130),
    'b' -> PricingRule(30, 2, 45),
    'c' -> PricingRule(20, 1, 20),
    'd' -> PricingRule(15, 1, 15)
  )


  val pricingRules = extractPricingRules(args)

  val input = StdIn.readLine("Please provide a list of items you wish to buy:")

  if (input.isEmpty) {
    println("No items provided.")
  } else {
    val items = input.mkString("").toList
    val checkout = new Checkout(pricingRules)
    val itemsMap = checkout.countItemsToMap(items)
    val totalPrice = checkout.calculateTotalPrice(itemsMap)
    println(s"Total price for items ${input.mkString("")}: £$totalPrice") // make into pounds and pence, with te correct amount of zeros (£1.00 fore example.)
    }

}