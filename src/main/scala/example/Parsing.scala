package example

import scala.language.higherKinds
import scala.util.matching.Regex


// List[A]
// Parsing[List[_]]

// paraboiled2

trait Parsing[Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def succeed[A](a: A): Parser[A]

  def string(s: String): Parser[String]

  def slice[A](p: Parser[A]): Parser[String]

  def attempt[A](p: Parser[A]): Parser[A]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def regex(r: Regex): Parser[String]





  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = {
    p.flatMap(f andThen succeed)
  }

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
    for {
      a <- p1
      b <- p2
    } yield (a, b)
  }

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    (p1 ** p2) map f.tupled
  }

  def many[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))(_ :: _) | succeed(List())
  }

  def skipL[A, B](p1: Parser[A], p2: => Parser[B]): Parser[B] = {
    map2(p1, p2)((_, b) => b)
  }

  def skipR[A, B](p1: Parser[A], p2: => Parser[B]): Parser[A] = {
    map2(p1, p2)((a, _) => a)
  }

  def surround[X, Y, A](start: Parser[X], stop: Parser[Y])(p: Parser[A]): Parser[A] = {
    start *> p <* stop
  }

  def matching(s: String): Parser[String] = regex(s.r)

  def whitespace: Parser[String] = matching("\\s*")

  def eof: Parser[String] = matching("\\z")

  def token[A](p: Parser[A]): Parser[A] = p <* whitespace

  def thru(end: String): Parser[String] = matching(".*?" + Regex.quote(end))

  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  def double: Parser[Double] = {
    token(matching("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?")).map(_.toDouble)
  }

  def sep[A, B](p: Parser[A], sp: Parser[B]): Parser[List[A]] = {
    map2(p, many(sp *> p))(_ :: _) | succeed(List())
  }

  implicit class ParserSyntax[A](p: Parser[A]) {
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def |[B >: A](p2: => Parser[B]): Parser[B] = p or p2
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)
    def <*[B](p2: => Parser[B]): Parser[A] = self.skipR(p, p2)
    def as[B](b: B): Parser[B] = p *> succeed(b)
  }
}
