package schnappi.full

import schnappi.core
import schnappi.core.Identifier
import schnappi.core.UniqueIdentifier
import scala.collection.immutable.HashMap
import scala.collection.immutable.LazyList
import scala.language.implicitConversions

sealed abstract class Err(x: String) extends Exception(x)

final case class CoreErr(x: core.Err) extends Err(s"core err $x")

sealed trait Exp {

}

final case class Context() // todo

type UnifySubstitution = HashMap[Hole, Core]

implicit class UnifySubstitutionOps(x: UnifySubstitution) {
  def addNoDup(k: Hole, v: Core): UnifySubstitution = if (x.contains(k)) throw new IllegalArgumentException() else x.updated(k, v)
}

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

  def or(y: Elab[T]): Elab[T] = state => mplus(x(state), y(state))
}

object Elab {
  def pure[T](x: T): Elab[T] = st => LazyList(ElabOk(st, x))
}

type Unifier[T] = UnifySubstitution => (UnifySubstitution, T)
implicit def unifier2UnifierOption[T](x: Unifier[T]): UnifierOption[T] = st => x(st) match {
  case (newst, x) => (newst, Some(x))
}

implicit class UnifierOps[T](x: Unifier[T]) {
  def flatMap[U](f: T => UnifierOption[U]): UnifierOption[U] = unifier2UnifierOption(x).flatMap(f)
}

type UnifierOption[T] = UnifySubstitution => (UnifySubstitution, Option[T])

implicit class UnifierOptionOps[T](x: UnifierOption[T]) {
  def flatMap[U](f: T => UnifierOption[U]): UnifierOption[U] = state => x(state) match {
    case (st, Some(v)) => f(v)(st)
    case (st, None) => (st, None)
  }

  def map[U](f: T => U): UnifierOption[U] = state => x(state) match {
    case (st, Some(v)) => (st, Some(f(v)))
    case (st, None) => (st, None)
  }

  def flatMapOrElse[U](f: T => UnifierOption[U], default: => UnifierOption[U]): UnifierOption[U] = state => x(state) match {
    case (st, Some(v)) => f(v)(st)
    case (st, None) => default(st)
  }
}

object UnifierOption {
  def pure[T](x: T): UnifierOption[T] = st => (st, Some(x))

  def fail[T]: UnifierOption[T] = st => (st, None)
}

object Unifier {
  def pure[T](x: T): Unifier[T] = st => (st, x)
}

sealed trait Core {
  def check(context: Context, t: Core): Elab[core.Core] = ???

  def infer(context: Context): Elab[(Core, core.Core)] = ???

  final def unify(other: Core): UnifierOption[Unit] = if (this == other) UnifierOption.pure(()) else for {
    self <- this.walk
    other <- other.walk
    _ <- if (self == other) {
      UnifierOption.pure(())
    } else (self, other) match {
      case (self: Hole, other) => self.impl_unify(other)
      case (self, other: Hole) => other.impl_unify(self)
      case (self, other) => self.impl_unify(other)
    }
  } yield ()

  def impl_unify(other: Core): UnifierOption[Unit] = ???

  def walk: Unifier[Core] = Unifier.pure(this)
}

final case class Hole(id: Identifier, uid: UniqueIdentifier) extends Core {
  override def walk: Unifier[Core] = state => (state, state.getOrElse(this, this))

  override def impl_unify(other: Core): UnifierOption[Unit] = state => (state.addNoDup(this, other), Some(()))
}
