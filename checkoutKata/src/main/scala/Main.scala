import scala.io.StdIn

object Main extends App {

  println(commandLineLogic.welcomeMessage())
  val pricingRules = commandLineLogic.extractPricingRules(args)
  commandLineLogic.delay(2000)
  val input = StdIn.readLine("Please provide a list of items you wish to buy:")
  commandLineLogic.processCheckout(pricingRules, input)
  commandLineLogic.delay(2000)
  println(commandLineLogic.outputMessage(input))
  commandLineLogic.delay(2000)
  println(commandLineLogic.thanksForShopping())

}
