package schnappi.full

import schnappi.core
import schnappi.core.Identifier
import schnappi.core.UniqueIdentifier
import scala.collection.immutable.HashMap
import scala.collection.immutable.LazyList

sealed abstract class Err(x: String) extends Exception(x)

final case class CoreErr(x: core.Err) extends Err(s"core err $x")

sealed trait Exp {

}

final case class Context() // todo

type UnifySubstitution = HashMap[Hole, Core]

type State = UnifySubstitution

final case class ElabOk[T](st: State, x: T)

type Elab[T] = State => LazyList[ElabOk[T] | Err]
private def mplus[T](x: LazyList[T], y: LazyList[T]): LazyList[T] = if (x.isEmpty) y else x.head #:: mplus(y, x.tail)
private def fairFlatten[T](x: LazyList[LazyList[T]]): LazyList[T] = if (x.isEmpty) LazyList() else mplus(x.head, fairFlatten(x.tail))

implicit class ElabOps[T](x: Elab[T]) {
  def flatMap[U](f: T => Elab[U]): Elab[U] = state => fairFlatten(x(state).map({
    case ElabOk(st, x) => f(x)(st)
    case e: Err => LazyList(e)
  }))
}

object Elab {
  def pure[T](x: T): Elab[T] = st => LazyList(ElabOk(st, x))
}

sealed trait Core {
  def check(context: Context, t: Core): Elab[core.Core] = ???

  def infer(context: Context): Elab[(Core, core.Core)] = ???

  def unify(other: Core): Option[UnifySubstitution] = ???

  def walk: Elab[Core] = ???
}

final case class Hole(id: Identifier, uid: UniqueIdentifier) extends Core