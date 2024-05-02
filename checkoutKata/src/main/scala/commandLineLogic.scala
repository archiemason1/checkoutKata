import Main.pricingRules
import checkoutKataCode.{Checkout, PricingRule}

object commandLineLogic {

  def extractPricingRules(args: Array[String]): Map[Char, PricingRule] = {
    if (args.length < 1) {
      delay(2000)
      println("No pricing rules provided. Using default rules.")
      defaultPricingRules
    } else {
      val rulesStr = args(0).split(",")
      val parsedRules = rulesStr.flatMap(parseRule(_, defaultPricingRules))
      val mergedRules = defaultPricingRules ++ parsedRules.toMap
      mergedRules
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

  def ruleParamInt(ruleParams: Array[String], element: Int): Int = {
    ruleParams(element).toInt
  }

  def ruleParamLetter(ruleParams: Array[String], element: Int, element2: Int): Option[Char] = {
    try {
      val item = ruleParams(element).charAt(element2)
      if (!item.isLetter) {
        println("Invalid item name. The first argument should be a letter.")
        None
      } else {
        Some(item)
      }
    } catch {
      case _: IndexOutOfBoundsException =>
        println("Invalid argument count. Expected format: <item> <price> <amount for specialPrice> <specialPrice>")
        None
    }
  }

  def createPricingRule(ruleParams: Array[String]): Option[(Char, PricingRule)] = {
    try {
      val itemOpt = ruleParamLetter(ruleParams, 0, 0)
      itemOpt.flatMap { item =>
        val unitPrice = ruleParamInt(ruleParams, 1)
        val amountToQualify = ruleParamInt(ruleParams, 2)
        val specialOfferPrice = ruleParamInt(ruleParams, 3)
        Some(item -> PricingRule(unitPrice, amountToQualify, specialOfferPrice))
      }
    } catch {
      case _: NumberFormatException =>
        println("Invalid number format. Price, amount, and special offer price must be integers.")
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

  def penniesToPoundsAndPennies(pennies: Int): String = {
    val pounds = pennies / 100
    val remainingPennies = pennies % 100
    f"£$pounds.$remainingPennies%02d"
  }

  def itemsToString(input: String): String = {
    input.mkString("")
  }

  def itemsToList(input: String): List[Char] = {
    input.mkString("").toList
  }

  def totalPriceOutput(input: String): String = {
    val items = itemsToString(input)
    val itemsList = itemsToList(input)
    val checkout = new Checkout(pricingRules)
    val validItems = items.filter(pricingRules.contains)
    val itemsMap = checkout.countItemsToMap(itemsList)
    val totalPrice = checkout.calculateTotalPrice(itemsMap)
    s"Total price for items ${validItems}: ${penniesToPoundsAndPennies(totalPrice)}"
  }

  def welcomeMessage(): String = {
    "Welcome to the checkout system."
  }

  def processCheckout(pricingRules: Map[Char, PricingRule], input: String): Unit = {
    if (pricingRules.isEmpty) {
      delay(2000)
      println("No pricing rules provided. Using default rules.")
    }
  }

  def outputMessage(input: String): String = {
    if (input.isEmpty) {
      "No items provided"
    } else {
      totalPriceOutput(input)
    }
  }

  def thanksForShopping(): String = {
    "Thanks for shopping at Archie's Checkout, have a good day!"
  }

  def delay(milliseconds: Long): Unit = {
    Thread.sleep(milliseconds)
  }

}
