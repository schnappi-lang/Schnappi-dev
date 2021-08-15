package schnappi.core

import scala.language.postfixOps
import cats.parse.{Parser0, Parser => P, Numbers}
import schnappi.core.Exps._

object Parser {
  private[this] val whitespaceChar: P[Unit] = P.charIn(" \t\r\n").void
  private[this] val whitespaces: P[Unit] = whitespaceChar.rep.void
  private[this] val whitespacesMaybe: Parser0[Unit] = (whitespaces ?).void

  // [\u4e00-\u9fa5] is Chinese chars
  private val atomRegex = "(\\w|[-？?/*:><_]|[\u4e00-\u9fa5])+".r

  private def isAtomChar(c: Char) = atomRegex.matches(c.toString)

  val parser: P[Exp] = P.recursive[Exp] { parser =>
    val whitespacesParser: P[Exp] = whitespaces *> parser

    def whitespacesPrefix[A](pa: P[A]): P[A] = whitespaces *> pa

    def product3[A, B, C](pa: P[A], pb: P[B], pc: P[C]): P[(A, B, C)] = (pa ~ pb ~ pc).map({ case ((a, b), c) => (a, b, c) })

    def taggedList2[A, B, T](tag: P[T], pa: P[A], pb: P[B]): P[(A, B)] = (whitespacesMaybe.with1 ~ tag *> (whitespacesPrefix(pa) ~ whitespacesPrefix(pb))).between(P.string("("), P.string(")")).backtrack

    def taggedList3[A, B, C, T](tag: P[T], pa: P[A], pb: P[B], pc: P[C]): P[(A, B, C)] = (whitespacesMaybe.with1 ~ tag *> product3(whitespacesPrefix(pa), whitespacesPrefix(pb), whitespacesPrefix(pc))).between(P.string("("), P.string(")")).backtrack

    def list3[A, B, C](pa: P[A], pb: P[B], pc: P[C]) = product3(whitespacesPrefix(pa), whitespacesPrefix(pb), whitespacesPrefix(pc)).between(P.string("("), P.string(")"))

    def listof[A](pa: P[A]): P[List[A]] = pa.repSep0(whitespaces).surroundedBy(whitespacesMaybe).with1.between(P.string("("), P.string(")"))

    def f1(tag: String): P[Exp] = parser.surroundedBy(whitespacesMaybe).between(P.string("(" + tag), P.string(")"))

    def f2(tag: String): P[(Exp, Exp)] = (parser ~ whitespacesParser).surroundedBy(whitespacesMaybe).between(P.string("(" + tag), P.string(")"))

    val identifer: P[String] = P.charsWhile(isAtomChar)

    val varp: P[Var] = (P.string("$") *> identifer).map(x => Var(Symbol(x)))
    val quotep: P[Quote] = (P.string("'") *> identifer).map(x => Quote(Symbol(x)))

    P.oneOf(List(
      P.string("zero").as(Zero()),
      f1("succ").map(Succ(_)),
      P.string("nat").as(Nat()),
      P.string("universe").as(Universe()),
      P.string("kind").as(Kind()),
      f1("makeKind").map(MakeKind(_)),
      f2("withAttrSize").map(WithAttrSize(_, _)),
      f1("withAttrUnknownFinite").map(WithAttrUnknownFinite(_)),
      f2("cons").map(Cons(_, _)),
      f1("car").map(Car(_)),
      f1("cdr").map(Cdr(_)),
      taggedList3(P.string("sigma"), parser, varp, parser).map(Sigma(_, _, _)),
      taggedList2(P.string("lambda"), varp, parser).map(Lambda(_, _)),
      taggedList3(P.string("pi"), parser, varp, parser).map(Pi(_, _, _)),
      taggedList3(P.string("recPi"), parser, varp, parser).map(RecPi(_, _, _)),
      taggedList3(P.string("rec"), varp, parser, parser).map(Rec(_, _, _)),
      taggedList2(P.string("recs"), listof(list3(varp, parser, parser)), parser).map(x => Recs(Set.empty.concat(x._1.map(Rec(_, _, _))), x._2)),
      f2("apply").map(Apply(_, _)),
      f2("the").map(The(_, _)),
      quotep,
      varp,
      P.string("atom").as(Atom()),
    ))
  }
  val fullParser: P[Exp] = parser <* whitespacesMaybe

  def parse(x: String): Either[P.Error, Exp] = fullParser.parseAll(x)

  def parseThrows(x: String): Exp = parse(x) match {
    case Right(x) => x
    case Left(e) => throw new IllegalArgumentException(e.toString)
  }
}

def parse(x: String) = Parser.parse(x)
def parseThrows(x: String) = Parser.parseThrows(x)
