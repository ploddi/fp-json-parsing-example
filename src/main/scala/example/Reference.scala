package example

import example.ReferenceTypes._

import scala.util.matching.Regex

object Reference extends Parsing[Parser] {

  def run[A](p: Parser[A])(s: String): Either[ParseError, A] = {
    val s0 = ParseState(Location(s))
    p(s0).extract
  }

  def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  def or[A](p: Parser[A], p2: => Parser[A]): Parser[A] =
    s => p(s) match {
      case Failure(e, false) => p2(s)
      case r => r // committed failure or success skips running `p2`
    }

  def flatMap[A, B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
    s => f(s) match {
      case Success(a, n) => g(a)(s.advanceBy(n))
        .addCommit(n != 0)
        .advanceSuccess(n)
      case f@Failure(_, _) => f
    }

  def string(w: String): Parser[String] = {
    val msg = "'" + w + "'"
    s => {
      val i = firstNonmatchingIndex(s.loc.input, w, s.loc.offset)
      if (i == -1) // they matched
        Success(w, w.length)
      else
        Failure(s.loc.advanceBy(i).toError(msg), i != 0)
    }
  }

  def regex(r: Regex): Parser[String] = {
    val msg = "regex " + r
    s =>
      r.findPrefixOf(s.input) match {
        case None => Failure(s.loc.toError(msg), isCommitted = false)
        case Some(m) => Success(m, m.length)
      }
  }

  def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit

  def slice[A](p: Parser[A]): Parser[String] =
    s => p(s) match {
      case Success(_, n) => Success(s.slice(n), n)
      case f@Failure(_, _) => f
    }
}