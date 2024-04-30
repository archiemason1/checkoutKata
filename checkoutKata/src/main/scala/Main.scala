import checkoutKataCode.{Checkout, PricingRule}

import scala.io.StdIn

object Main extends App {

  def extractPricingRules(args: Array[String]): Map[Char, PricingRule] = {
    if (args.length < 1) {
      println("No pricing rules provided. Using default rules.")
      defaultPricingRules
    } else {
      val rulesStr = args(0).split(",")
      val parsedRules = rulesStr.flatMap(parseRule(_, defaultPricingRules))
      val mergedRules = defaultPricingRules ++ parsedRules.toMap
      if (mergedRules.isEmpty) {
        println("No valid pricing rules provided. Using default rules.")
        defaultPricingRules
      } else {
        mergedRules
      }
    }
  }

  def parseRule(ruleStr: String, existingRules: Map[Char, PricingRule]): Option[(Char, PricingRule)] = {
    val ruleParams = ruleStr.trim.split("\\s+")
    if (ruleParams.length != 4) {
      printInvalidFormatMessage(ruleStr)
      None
    } else {
      createPricingRule(ruleParams) match {
        case Some(newRule) =>
          val item = newRule._1
          val updatedRules = existingRules + newRule
          Some(item -> updatedRules.getOrElse(item, defaultPricingRules(item)))
        case None => None
      }
    }
  }

  def createPricingRule(ruleParams: Array[String]): Option[(Char, PricingRule)] = {
    try {
      val item = ruleParams(0).charAt(0)
      if (!item.isLetter) {
        println("Invalid item name. The first argument should be a letter.")
        None
      } else {
        val unitPrice = ruleParams(1).toInt
        val amountToQualify = ruleParams(2).toInt
        val specialOfferPrice = ruleParams(3).toInt
        Some(item -> PricingRule(unitPrice, amountToQualify, specialOfferPrice))
      }
    } catch {
      case _: NumberFormatException =>
        println("Invalid number format. Price, amount, and special offer price must be integers.")
        None
      case _: IndexOutOfBoundsException =>
        println("Invalid argument count. Expected format: <item> <price> <amount for specialPrice> <specialPrice>")
        None
    }
  }

  def printInvalidFormatMessage(ruleStr: String): Unit = {
    println(
      s"""Invalid pricing rule format: $ruleStr. Expected format:
         |<item> <price> <amount for specialPrice> <specialPrice>
         |e.g run "a 20 2 35" item -> a, price -> £0.20, 2 for -> £0.35
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
      val items = input.mkString("")
      val itemsList = items.toList
      val checkout = new Checkout(pricingRules)
      val validItems = items.filter(pricingRules.contains)
      val itemsMap = checkout.countItemsToMap(itemsList)
      val totalPrice = checkout.calculateTotalPrice(itemsMap)
      println(s"Total price for items ${validItems}: ${penniesToPoundsAndPennies(totalPrice)}")
    }
  }

  totalPriceOutput(input)
}
