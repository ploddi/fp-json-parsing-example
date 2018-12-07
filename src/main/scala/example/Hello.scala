package example

object Hello extends App {

  val js1 =
    """{
      |    "Company name" : "Microsoft Corporation",
      |    "Ticker"  : "MSFT",
      |    "Active"  : true,
      |    "Price"   : 30.66,
      |    "Shares outstanding" : 8.38e9,
      |    "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
      | }""".stripMargin


  import Reference.run

  val jsonParser = Json.parser(Reference)

  val result = run(jsonParser)(js1)

  result match {
    case Left(error) => println(error)
    case Right(json) => println(json)
  }

}

