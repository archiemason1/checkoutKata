import checkoutKataCode.{Checkout, PricingRule}

import scala.io.StdIn

object Main {
  def extractPricingRules(args: Array[String]): Map[Char, PricingRule] = {
    if (args.length < 1) {
      println("No pricing rules provided. Using default rules.")
      return defaultPricingRules
    }

    val rulesStr = args(0).split(",")

    val pricingRules = rulesStr.flatMap { ruleStr =>
      val ruleParams = ruleStr.trim.split("\\s+")
      if (ruleParams.length != 4) {
        println(s"Illegal pricing rule format: $ruleStr. Skipping. Try inputting the" +
          s"pricing rules in the format \"a 30 2 55, b...\" where a -> item, 30 -> single price" +
          s"2 -> amount to qualify special price, 55 -> special price")
        None
      } else {
        try {
          val item = ruleParams(0).charAt(0)
          val unitPrice = ruleParams(1).toInt
          val amountToQualify = ruleParams(2).toInt
          val specialOfferPrice = ruleParams(3).toInt
          Some(item -> PricingRule(unitPrice, amountToQualify, specialOfferPrice))
        } catch {
          case _: NumberFormatException =>
            println(s"Illegal pricing rule format: $ruleStr. Skipping. Try inputting the" +
              s"pricing rules in the format \"a 30 2 55, b...\" where a -> item, 30 -> single price" +
              s"2 -> amount to qualify special price, 55 -> special price")
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

  def main(args: Array[String]): Unit = {
    val pricingRules = extractPricingRules(args)

    val input = StdIn.readLine("Please provide a list of items you wish to buy:")

    if (input.isEmpty) {
      println("No items provided.")
    } else {
      val items = input.mkString("").toList
      val checkout = new Checkout(pricingRules)
      val itemsMap = checkout.countItemsToMap(items)
      val totalPrice = checkout.calculateTotalPrice(itemsMap)
      println(s"Total price for items ${input.mkString("")}: £$totalPrice")
    }
  }
}