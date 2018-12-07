package example

import scala.language.{higherKinds, implicitConversions}

// Int, Boolean  *
// List[A] * -> *
// (A, B) (Int, Boolean)
// Either[A, B] Left(A) Right(B)
// A | B
// A | B | C


sealed trait Json

case object JsNull extends Json
case class JsBoolean(boolean: Boolean) extends Json
case class JsNumber(double: Double) extends Json
case class JsString(string: String) extends Json
case class JsArray(list: List[Json]) extends Json
case class JsObject(map: Map[String, Json]) extends Json

// AST Json

object Json {



  def parser[Parser[+_]](P: Parsing[Parser]): Parser[Json] = {
    import P._

    implicit def toSyntax[A](p: A)(implicit f: A => Parser[String]): ParserSyntax[String] = f(p)
    implicit def toToken(s: String): Parser[String] = token(string(s))


    def litNull = "null".as(JsNull)
    def litTrue = "true".as(JsBoolean(true))
    def litFalse = "false".as(JsBoolean(false))
    def litBoolean = litTrue | litFalse
    def litNum = token(double).map(JsNumber)
    def litString = token(quoted).map(JsString)

    def lit = litNull | litBoolean | litNum | litString

    // [ value1 , value2 ]
    def arr = surround("[", "]")(sep(value, ",")).map(JsArray)

    def obj = {
      surround("{", "}")(sep( token(quoted) ** (":" *> value) , ",")).map { fields =>
        JsObject(fields.toMap)
      }
    }

    def value: Parser[Json] = lit | arr | obj

    def json: Parser[Json] = whitespace *> value <* eof


    json
  }

}