import checkoutKataCode.{Checkout, PricingRule}

import scala.io.StdIn

object Main extends App {

  def extractPricingRules(args: Array[String]): Map[Char, PricingRule] = {
    if (args.length < 1) {
      println("No pricing rules provided. Using default rules.")
      defaultPricingRules
    } else {
      val rulesStr = args(0).split(",")
      val pricingRules = rulesStr.flatMap(parseRule).toMap

      if (pricingRules.isEmpty) {
        println("No valid pricing rules provided. Using default rules.")
        defaultPricingRules
      } else {
        pricingRules
      }
    }
  }

  def parseRule(ruleStr: String): Option[(Char, PricingRule)] = {
    val ruleParams = ruleStr.trim.split("\\s+")
    if (ruleParams.length != 4) {
      printInvalidFormatMessage(ruleStr)
      None
    } else {
      try {
        createPricingRule(ruleParams)
      } catch {
        case _: NumberFormatException =>
          printInvalidFormatMessage(ruleStr)
          None
      }
    }
  }

  def createPricingRule(ruleParams: Array[String]): Option[(Char, PricingRule)] = {
    val item = ruleParams(0).charAt(0)
    val unitPrice = ruleParams(1).toInt
    val amountToQualify = ruleParams(2).toInt
    val specialOfferPrice = ruleParams(3).toInt
    Some(item -> PricingRule(unitPrice, amountToQualify, specialOfferPrice))
  }

  def printInvalidFormatMessage(ruleStr: String): Unit = {
    println(
      s"""Illegal pricing rule format: $ruleStr. Skipping. Try inputting the pricing rules
         |in the format \"a 30 2 55, b...\" where a -> item, 30 -> single price of £0.30
         |2 -> amount to qualify special price, 55 -> special price of £0.55 for 2 of the item")
         |""".stripMargin)
  }

  def defaultPricingRules: Map[Char, PricingRule] = Map(
    'a' -> PricingRule(50, 3, 130),
    'b' -> PricingRule(30, 2, 45),
    'c' -> PricingRule(20, 1, 20),
    'd' -> PricingRule(15, 1, 15)
  )

  println("Welcome to the checkout system.")

  val pricingRules = extractPricingRules(args)

  if (pricingRules.isEmpty) {
    println("No pricing rules provided. Using default rules.")
  }

  val input = StdIn.readLine("Please provide a list of items you wish to buy:")

  def penniesToPoundsAndPennies(pennies: Int): String = {
    val pounds = pennies / 100
    val remainingPennies = pennies % 100
    f"£$pounds.$remainingPennies%02d"
  }

  def totalPriceOutput(input: String): Unit = {
    if (input.isEmpty) {
      println("No items provided.")
    } else {
      val items = input.mkString("").toList
      val checkout = new Checkout(pricingRules)
      val itemsMap = checkout.countItemsToMap(items)
      val totalPrice = checkout.calculateTotalPrice(itemsMap)
      println(s"Total price for items ${input.mkString("")}: ${penniesToPoundsAndPennies(totalPrice)}")
    }
  }

  totalPriceOutput(input)
}
