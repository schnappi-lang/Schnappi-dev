package schnappi.core

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

type Identifier = Symbol

type UniqueIdentifier = Int

type NaturalNumber = Int

final case class Error(x: String) extends Exception(x)

sealed abstract class Err(x: String) extends Exception(x)

final case class ErrCantInfer(context: Context, x: Core) extends Err(s"can't infer $x in the context $context")

final case class ErrCheckFailed(context: Context, expectedType: Core, x: Core, realType: Core) extends Err(s"check $x failed in the context $context expect $expectedType, got $realType")

final case class ErrExpected(context: Context, expectedType: String, x: Core, realType: Core) extends Err(s"expect $x to be $expectedType in the context $context, got $realType")

final case class ErrExpectedV(context: Context, expectedType: String, x: Core) extends Err(s"expect $x to be $expectedType in the context $context")

final case class ErrTypeUnknown(context: Context, x: Cores.AccessVar) extends Err(s"the type of $x is unknown in the context $context")

final case class ErrCantEvalToType(context: Context, x: Core) extends Err(s"$x in the context $context can't be a type")

final case class ErrLetrec(context: Context, x: Core) extends Err(s"illegal letrec $x in the context $context")

final case class ErrDiverge(context: Context, x: Cores.Rec) extends Err(s"expected diverge for $x in the context $context")

final case class ErrUnknownFiniteRec(context: Context, x: Core, t: Core) extends Err(s"don't know how to check UnknownFinite $x: $t in $context")

final case class ErrNotDivergePiRec(context: Context, x: Core, t: Core) extends Err(s"don't know how to check non-diverge Pi $x: $t in $context")

final case class ErrRecs(context: Context, errs: List[Err]) extends Err(s"Recs failed in $context caused by $errs")

final case class ErrUnknownTypeRec(context: Context, x: Core, t: Core) extends Err(s"don't know how to check  $x: $t with unknown type in $context")

final case class ErrExpectedCodata(context: Context, x: Core, t: Core) extends Err(s"expected $x to be codata in the context $context, got $t")


final case class ErrPlainSubtype(t: Core, sub: Core) extends Err(s"$sub can't be a plain subtype of $t")

final case class ErrWeakSubtype(t: Core, sub: Core) extends Err(s"$sub can't be a weak subtype of $t")

type Maybe[T] = Either[Err, T]
private implicit def someToRight[T, U](x: Some[T]): Right[U, T] = x match {
  case Some(x) => Right(x)
}
private implicit def eitherToBoolean[T, U](x: Either[T, U]): Boolean = x match {
  case Right(_) => true
  case Left(_) => false
}
private implicit def eitherErase[T, U](x: Either[T, U]): Either[T, Unit] = x match {
  case Right(_) => Right(())
  case Left(v) => Left(v)
}

private implicit final class EitherAnd[T, U](self: Either[T, U]) {
  def and[U1](other: => Either[T, U1]): Either[T, (U, U1)] = self match {
    case Left(x) => Left(x)
    case Right(a) => other match {
      case Left(x) => Left(x)
      case Right(b) => Right((a, b))
    }
  }
}

// todo - add `.c`
sealed trait MaybeSt[T] {
  def flatMap[U](f: T => MaybeSt[U]): MaybeSt[U] = this match {
    case MaybeStOk(usages1, x) => f(x) match {
      case MaybeStOk(usages2, res) => MaybeStOk(usages1.concat(usages2), res)
      case e@MaybeStErr(_) => e
    }
    case MaybeStErr(err) => MaybeStErr(err)
  }

  def map[U](f: T => U): MaybeSt[U] = flatMap(x => MaybeSt.pure(f(x)))

  def c: MaybeSt[T] = this match {
    case MaybeStOk(_, x) => MaybeStOk(Set(), x)
    case x@MaybeStErr(_) => x
  }

  def and[U](other: MaybeSt[U]): MaybeSt[Unit] = for {
    _ <- this
    _ <- other
  } yield ()

  def toOption: Option[T] = this match {
    case MaybeStOk(_, x) => Some(x)
    case MaybeStErr(_) => None
  }

  def getOrElse[B >: T](default: => B): B = this.toOption.getOrElse({
    default
  })

  def isRight: Boolean = this match {
    case MaybeStOk(_, _) => true
    case MaybeStErr(_) => false
  }

  def isLeft: Boolean = this match {
    case MaybeStOk(_, _) => false
    case MaybeStErr(_) => true
  }
}

object MaybeSt {
  private[core] def pure[T](x: T): MaybeSt[T] = MaybeStOk(Set(), x)
}

final case class MaybeStErr[T](err: Err) extends MaybeSt[T]

final case class MaybeStOk[T](notErasedUsages: Set[Cores.AccessVar], x: T) extends MaybeSt[T]

private implicit def maybeToMaybeSt[T](x: Either[Err, T]): MaybeSt[T] = x match {
  case Left(e) => MaybeStErr(e)
  case Right(x) => MaybeStOk(Set(), x)
}

private implicit class MaybeToMaybeSt[T](x: Either[Err, T]) {
  def l: MaybeSt[T] = x match {
    case Left(e) => MaybeStErr(e)
    case Right(x) => MaybeStOk(Set(), x)
  }
}

private implicit class MaybeStTraverse[T](x: List[MaybeSt[T]]) {
  def traverse: MaybeSt[List[T]] = x match {
    case Nil => MaybeSt.pure(Nil)
    case x :: xs => for {
      x1 <- x
      xs1 <- xs.traverse
    } yield x1 :: xs1
  }
}

object MaybeStRight {
  def unapply[T](x: MaybeSt[T]): Option[T] = x match {
    case MaybeStOk(_, v) => Some(v)
    case _ => None
  }

  def unapply[T](x: MaybeStOk[T]): Some[T] = Some(x.x)
}

object UniqueIdentifier {
  // todo: add this to MaybeSt to make the checker purely functional
  private var count: UniqueIdentifier = 0

  def gen: UniqueIdentifier = this.synchronized {
    val result = count
    count = count + 1
    result
  }
}

final case class VarId(id: Identifier, uid: UniqueIdentifier)

object VarId {
  def gen(id: Identifier): VarId = VarId(id, UniqueIdentifier.gen)
}

final case class Context(context: HashMap[VarId, (Type, Option[Core])], recSize: Option[Core]) {
  def updated(id: VarId, t: Type, v: Option[Core]): Context = Context(context.updated(id, (t, v)), recSize)

  def updated(id: Cores.Var, t: Type, v: Option[Core]): Context = this.updated(id.x, t, v)

  def updated(id: VarId, t: Type, v: Core): Context = Context(context.updated(id, (t, Some(v))), recSize)

  def updated(id: Cores.Var, t: Type, v: Core): Context = this.updated(id.x, t, v)

  def updated(id: VarId, t: Type): Context = Context(context.updated(id, (t, None)), recSize)

  def updated(id: Cores.Var, t: Type): Context = this.updated(id.x, t)

  def get(id: VarId): Option[(Type, Option[Core])] = context.get(id)

  def get(id: Cores.Var): Option[(Type, Option[Core])] = this.get(id.x)

  def getType(id: VarId): Option[Type] = context.get(id).map(_._1)

  def getType(v: Cores.Var): Option[Type] = this.getType(v.x)

  def getValue(id: VarId): Option[Core] = context.get(id).map(_._2).flatten

  def getValue(id: Cores.Var): Option[Core] = this.getValue(id.x)

  def concat(xs: List[(VarId, Type, Core)]): Context = xs match {
    case Nil => this
    case (id, t, v) :: xs => this.updated(id, t, v).concat(xs)
  }

  def setRecSize(size: Option[Core]): Context = Context(this.context, size)

  def setRecSize(size: Core): Context = this.setRecSize(Some(size))

  def clearRecSize: Context = this.setRecSize(None)

  def getRecSize: Option[Core] = recSize
}

object Context {
  val Empty = Context(HashMap(), None)
}

final case class AlphaMapping(inner: HashMap[VarId, VarId], reverseMap: HashMap[VarId, VarId]) {
  def has(a: VarId, b: VarId): Boolean = inner.get(a) match {
    case Some(b0) => b == b0
    case None => false
  }

  def add(a: VarId, b: VarId): AlphaMapping = inner.get(a) match {
    case Some(b0) => if (b == b0) this else throw Error("duplicate")
    case None => reverseMap.get(b) match {
      case Some(a0) => if (a == a0) throw Error("Illegal State") else throw Error("duplicate")
      case None => AlphaMapping(inner.updated(a, b), reverseMap.updated(b, a))
    }
  }

  def reverse: AlphaMapping = AlphaMapping(reverseMap, inner)
}

object AlphaMapping {
  val Empty: AlphaMapping = AlphaMapping(HashMap(), HashMap())
}

// uses Identifier
sealed trait Exp {
  def weakHeadNormalForm: Exp = ???

  def toCore(scope: HashMap[Identifier, VarId]): Core = this.toCore

  def toCore: Core = this.toCore(HashMap())
}

// is neutral if appers in normal form
sealed trait ExpNeu extends Exp

sealed trait AlphaEtaEqual {

}

// uses VarId
type Subst = HashMap[Cores.Var, Core]

sealed trait Core {
  def subst(s: Subst): Core

  final def scanVar(v: Cores.Var): NaturalNumber = ???

  def scan: List[Core]

  final def subst(v: Cores.Var, x: Core): Core = this.subst(HashMap((v, x)))

  final def subst(v: Cores.Var, t: Type, x: Core): Core = this.subst(HashMap((v, Cores.InternalThe(t, x))))

  def alpha_beta_eta_equals(other: Core, map: AlphaMapping): Boolean = this == other

  final def alpha_beta_eta_equals(other: Core): Boolean = this.alpha_beta_eta_equals(other, AlphaMapping.Empty)

  def weakHeadNormalForm(context: Context): Core = {
    val next = this.reduce(context)
    if (next == this) {
      this
    } else {
      next.weakHeadNormalForm(context)
    }
  }

  def weakHeadNormalForm: Core = this.weakHeadNormalForm(Context.Empty)

  def reduce(context: Context): Core = this
  // def reduce(context: Context): Maybe[Context, Core] = ???

  def impl_infer(context: Context): MaybeSt[Type] = {
    val next = this.reduce(context)
    if (next == this) {
      Left(ErrCantInfer(context, this))
    } else {
      next.infer(context)
    }
  }

  private def post_check[T](context: Context, t: Type, x: MaybeSt[T]): MaybeSt[T] = x match {
    case e@MaybeStErr(_) => e
    case res@MaybeStOk(st, v) => (t.isErased, t.isSelfErased) match {
      case (true, true) => MaybeStOk(Set(), v)
      case (false, true) => res
      case (true, false) => for {
        _ <- t.infer(context)
      } yield v
      case (false, false) => for {
        _ <- res
        _ <- t.infer(context)
      } yield v
    }
  }

  final def infer(context: Context): MaybeSt[Type] = impl_infer(context) match {
    case e@MaybeStErr(_) => e
    case res@MaybeStOk(_, t) => post_check(context, t, res)
  }

  final def infer: MaybeSt[Type] = this.infer(Context.Empty)

  def impl_check(context: Context, t: Type): MaybeSt[Unit] = this.infer(context) match {
    case MaybeStOk(st, t0) => if (t.subsetOrEqual(t0)) {
      MaybeStOk(st, ())
    } else {
      MaybeStErr(ErrCheckFailed(context, t, this, t0))
    }
    case MaybeStErr(err) => {
      val next = this.reduce(context)
      if (next == this) {
        MaybeStErr(err)
      } else {
        next.check(context, t)
      }
    }
  }

  final def check(context: Context, t: Type): MaybeSt[Unit] = impl_check(context, t) match {
    case e@MaybeStErr(_) => e
    case res@MaybeStOk(_, _) => post_check(context, t, res)
  }

  final def check(t: Type): MaybeSt[Unit] = this.check(Context.Empty, t)

  //if (this.check(context, Cores.UniverseInfinite)) {
  //  Some(Type(this.subst(context), ???))
  //} else
  def evalToType(context: Context): MaybeSt[Type] = {
    val next = this.reduce(context)
    if (next == this) {
      Left(ErrCantEvalToType(context, this))
    } else {
      next.evalToType(context)
    }
  }

  def evalToType: MaybeSt[Type] = evalToType(Context.Empty)

  final def reducingMatch[A](context: Context, f: Core => Option[A]): Option[A] = f(this) orElse {
    val next = this.reduce(context)
    if (next == this) {
      None
    } else {
      next.reducingMatch(context, f)
    }
  }

  final def reducingMatch[A](context: Context, f: Core => Maybe[A]): Maybe[A] = f(this) match {
    case Left(err) => {
      val next = this.reduce(context)
      if (next == this) {
        Left(err)
      } else {
        next.reducingMatch(context, f)
      }
    }
    case Right(v) => Right(v)
  }

  final def reducingMatch[A](context: Context, f: Core => MaybeSt[A]): MaybeSt[A] = f(this) match {
    case MaybeStErr(err) => {
      val next = this.reduce(context)
      if (next == this) {
        MaybeStErr(err)
      } else {
        next.reducingMatch(context, f)
      }
    }
    case v: MaybeStOk[A] => v
  }

  final def reducingMatch(context: Context, f: Core => Boolean): Boolean = f(this) || {
    val next = this.reduce(context)
    if (next == this) {
      false
    } else {
      next.reducingMatch(context, f)
    }
  }
}

sealed trait CoreUniverse extends Core {
  final override def impl_infer(context: Context): MaybeSt[Type] = this.evalToType(context).map(_.upperUniverse)
}

sealed trait CoreKind extends Core {
  final override def impl_infer(context: Context): MaybeSt[Type] = this.evalToType(context).map(_.upperUniverse)
}

// is neutral if appers in normal form
sealed trait CoreNeu extends Core

sealed trait Attr {
  def scan: List[Core] = List()

  def alpha_beta_eta_equals(other: Attr, map: AlphaMapping): Boolean = this == other

  final def alpha_beta_eta_equals(other: Attr): Boolean = this.alpha_beta_eta_equals(other, AlphaMapping.Empty)
}

private sealed trait NatParseResult

private case class NatParseResult_Just(x: NaturalNumber) extends NatParseResult

private case class NatParseResult_SuccNeu(x: NaturalNumber, neu: CoreNeu) extends NatParseResult

private def parseNat(x: Core): Option[NatParseResult] = x.weakHeadNormalForm match {
  case Cores.Zero() => Some(NatParseResult_Just(0))
  case Cores.Succ(x) => parseNat(x) map {
    case NatParseResult_Just(v) => NatParseResult_Just(v + 1)
    case NatParseResult_SuccNeu(v, neu) => NatParseResult_SuccNeu(v + 1, neu)
  }
  case neu: CoreNeu => Some(NatParseResult_SuccNeu(0, neu))
  case _ => None
}

private def natToCore(x: NaturalNumber, base: Core = Cores.Zero()): Core = if (x == 0) base else Cores.Succ(natToCore(x - 1))

def mergeTwoNat(x: Core, y: Core): Option[Core] = if (x == y) Some(x) else (for {x <- parseNat(x); y <- parseNat(y)} yield (x, y)) flatMap {
  case (NatParseResult_Just(x), NatParseResult_Just(y)) => Some(natToCore(x.max(y)))
  case (NatParseResult_SuccNeu(x, xNeu), NatParseResult_SuccNeu(y, yNeu)) => if (xNeu == yNeu) Some(natToCore(x.max(y), xNeu)) else None
  // following are required and work pretty well if y is zero
  case (NatParseResult_SuccNeu(x, xNeu), NatParseResult_Just(y)) => Some(natToCore(x.max(y), xNeu))
  case (NatParseResult_Just(y), NatParseResult_SuccNeu(x, xNeu)) => Some(natToCore(x.max(y), xNeu))
}

sealed trait AttrLevel extends Attr {
  def subst(s: Subst): AttrLevel = this match {
    case AttrLevel_Known(x) => AttrLevel_Known(x.subst(s))
    case other@(AttrLevel_UniverseInUniverse()) => other
  }

  def merge(other: AttrLevel): AttrLevel = (this, other) match {
    case (_, AttrLevel_UniverseInUniverse()) | (AttrLevel_UniverseInUniverse(), _) => AttrLevel_UniverseInUniverse()
    case (AttrLevel_Known(x), AttrLevel_Known(y)) => mergeTwoNat(x, y) match {
      case Some(r) => AttrLevel_Known(r)
      case None => AttrLevel_UniverseInUniverse()
    }
  }

  def upper: AttrLevel = this match {
    case AttrLevel_UniverseInUniverse() => AttrLevel_UniverseInUniverse()
    case AttrLevel_Known(level) => AttrLevel_Known(Cores.Succ(level))
  }
}

object AttrLevel {
  val Base = AttrLevel_Known(Cores.Zero())
}

final case class AttrLevel_UniverseInUniverse() extends AttrLevel

// zero - pure values - Nat ...
// one - Universe0 Kind0 ...
final case class AttrLevel_Known(level: Core) extends AttrLevel {
  override def scan: List[Core] = List(level)

  override def alpha_beta_eta_equals(other: Attr, map: AlphaMapping): Boolean = other match {
    case AttrLevel_Known(otherLevel) => level.alpha_beta_eta_equals(otherLevel, map)
    case _ => false
  }
}

sealed trait AttrSize extends Attr {
  def subst(s: Subst): AttrSize = this match {
    case AttrSize_Known(x) => AttrSize_Known(x.subst(s))
    case other@(AttrSize_Infinite() | AttrSize_UnknownFinite()) => other
  }

  def merge(other: AttrSize): AttrSize = (this, other) match {
    case (_, AttrSize_Infinite()) | (AttrSize_Infinite(), _) => AttrSize_Infinite()
    case (_, AttrSize_UnknownFinite()) | (AttrSize_UnknownFinite(), _) => AttrSize_UnknownFinite()
    case (AttrSize_Known(x), AttrSize_Known(y)) => mergeTwoNat(x, y) match {
      case Some(r) => AttrSize_Known(r)
      case None => AttrSize_UnknownFinite()
    }
  }

  def succ: AttrSize = this match {
    case AttrSize_Infinite() | AttrSize_UnknownFinite() => this
    case AttrSize_Known(x) => AttrSize_Known(Cores.Succ(x))
  }

  def getPlainSubtype(context: Context): Maybe[AttrSize] = this match {
    case AttrSize_Infinite() | AttrSize_UnknownFinite() => Right(this)
    case AttrSize_Known(x) => x.reducingMatch(context, {
      case Cores.Succ(s) => Right(AttrSize_Known(s))
      case _ => Left(???)
    })
  }
}

object AttrSize {
  val Base = AttrSize_Known(Cores.Zero())
}

final case class AttrSize_UnknownFinite() extends AttrSize

final case class AttrSize_Infinite() extends AttrSize

final case class AttrSize_Known(size: Core) extends AttrSize {
  override def scan: List[Core] = List(size)

  override def alpha_beta_eta_equals(other: Attr, map: AlphaMapping): Boolean = other match {
    case AttrSize_Known(otherSize) => size.alpha_beta_eta_equals(otherSize, map)
    case _ => false
  }
}

sealed trait AttrUsage extends Attr {
  def subst(s: Subst): AttrUsage = this

  def merge(other: AttrUsage): AttrUsage = (this, other) match {
    case (AttrUsage_Erased(), _) | (_, AttrUsage_Erased()) => AttrUsage_Erased()
    case (AttrUsage_Once(), _) | (_, AttrUsage_Once()) => AttrUsage_Once()
    case (AttrUsage_Unlimited(), AttrUsage_Unlimited()) => AttrUsage_Unlimited()
  }
}

object AttrUsage {
  val Base = AttrUsage_Unlimited()
}

final case class AttrUsage_Erased() extends AttrUsage

final case class AttrUsage_Once() extends AttrUsage

final case class AttrUsage_Unlimited() extends AttrUsage

sealed trait AttrSelfUsage extends Attr {
  def subst(s: Subst): AttrSelfUsage = this

  def merge(other: AttrSelfUsage): AttrSelfUsage = (this, other) match {
    case (AttrSelfUsage_Erased(), _) | (_, AttrSelfUsage_Erased()) => AttrSelfUsage_Erased()
    case (AttrSelfUsage_Once(), _) | (_, AttrSelfUsage_Once()) => AttrSelfUsage_Once()
    case (AttrSelfUsage_Unlimited(), AttrSelfUsage_Unlimited()) => AttrSelfUsage_Unlimited()
  }

  def upper: AttrUsage = this match {
    case AttrSelfUsage_Erased() => AttrUsage_Erased()
    case AttrSelfUsage_Once() => AttrUsage_Once()
    case AttrSelfUsage_Unlimited() => AttrUsage_Unlimited()
  }
}

object AttrSelfUsage {
  val Base = AttrSelfUsage_Unlimited()
}

final case class AttrSelfUsage_Erased() extends AttrSelfUsage

final case class AttrSelfUsage_Once() extends AttrSelfUsage

final case class AttrSelfUsage_Unlimited() extends AttrSelfUsage

final case class AttrAssumptions(assumptions: Set[Type]) extends Attr {
  override def scan: List[Core] = assumptions.toList

  def subst(s: Subst): AttrAssumptions = AttrAssumptions.nodupApply(assumptions.map(_.subst(s)))

  def merge(other: AttrAssumptions): AttrAssumptions = AttrAssumptions.nodupApply(assumptions.union(other.assumptions))

  override def alpha_beta_eta_equals(other: Attr, map: AlphaMapping): Boolean = other match {
    case AttrAssumptions(otherAssumptions) if assumptions.size == otherAssumptions.size => {
      val bs = otherAssumptions.toList
      assumptions.toList.permutations.exists((as) => as.zip(bs).forall({ case (x, y) => x.alpha_beta_eta_equals(y, map) }))
    }
    case _ => false
  }
}

object AttrAssumptions {
  def Base = AttrAssumptions(Set())

  private def distinct(xs: List[Type]): List[Type] = xs match {
    case Nil => Nil
    case x :: xs => x :: distinct(xs.filterNot(x.alpha_beta_eta_equals(_)))
  }

  private def checkErased(assumptions: Set[Type]): Boolean = assumptions.forall(_.attrs.usage == AttrUsage_Erased())

  private def nodupApply(assumptions: Set[Type]): AttrAssumptions = new AttrAssumptions(Set.empty.concat(distinct(assumptions.toList).map(_.erased)))

  def apply(assumptions: Set[Type]): AttrAssumptions = if (checkErased(assumptions)) {
    nodupApply(assumptions)
  } else {
    throw new IllegalArgumentException("non erased assumption!")
  }

  def safeApply(assumptions: Set[Type]): Maybe[AttrAssumptions] = if (checkErased(assumptions)) {
    Right(nodupApply(assumptions))
  } else {
    Left(???)
  }
}

sealed trait AttrDiverge extends Attr {
  def subst(s: Subst): AttrDiverge = this

  def merge(other: AttrDiverge): AttrDiverge = (this, other) match {
    case (AttrDiverge_Yes(), _) | (_, AttrDiverge_Yes()) => AttrDiverge_Yes()
    case (AttrDiverge_No(), AttrDiverge_No()) => AttrDiverge_No()
  }
}

object AttrDiverge {
  val Base = AttrDiverge_No()
}

final case class AttrDiverge_Yes() extends AttrDiverge

final case class AttrDiverge_No() extends AttrDiverge

// todo: size of a lambda?

final case class Attrs(level: AttrLevel, size: AttrSize, usage: AttrUsage, selfUsage: AttrSelfUsage, assumptions: AttrAssumptions, diverge: AttrDiverge) {
  def scan: List[Core] = level.scan ++ size.scan ++ usage.scan ++ selfUsage.scan ++ assumptions.scan ++ diverge.scan

  def subst(s: Subst): Attrs = Attrs(level.subst(s), size.subst(s), usage.subst(s), selfUsage.subst(s), assumptions.subst(s), diverge.subst(s))

  def merge(other: Attrs): Attrs = Attrs(level.merge(other.level), size.merge(other.size), usage.merge(other.usage), selfUsage.merge(other.selfUsage), assumptions.merge(other.assumptions), diverge.merge(other.diverge))

  // pi is not plain
  // plain: sigma either ...
  def validPlainSubtype(subtype: Attrs): Boolean =
    this.level.merge(subtype.level).alpha_beta_eta_equals(this.level) &&
      this.size.merge(subtype.size.succ).alpha_beta_eta_equals(this.size) &&
      this.usage.merge(subtype.usage).alpha_beta_eta_equals(this.usage) &&
      this.selfUsage.merge(subtype.selfUsage).alpha_beta_eta_equals(this.selfUsage) &&
      this.assumptions.merge(subtype.assumptions).alpha_beta_eta_equals(this.assumptions) &&
      this.diverge.merge(subtype.diverge).alpha_beta_eta_equals(this.diverge)

  def getPlainSubtype(context: Context): Maybe[Attrs] = size.getPlainSubtype(context).map(Attrs(level, _, usage, selfUsage, assumptions, diverge))

  def validWeakSubtype(subtype: Attrs): Boolean = this.level.merge(subtype.level).alpha_beta_eta_equals(this.level)

  def alpha_beta_eta_equals(other: Attrs, map: AlphaMapping): Boolean =
    level.alpha_beta_eta_equals(other.level, map) &&
      size.alpha_beta_eta_equals(other.size, map) &&
      usage.alpha_beta_eta_equals(other.usage, map) &&
      selfUsage.alpha_beta_eta_equals(other.selfUsage, map) &&
      assumptions.alpha_beta_eta_equals(other.assumptions, map) &&
      diverge.alpha_beta_eta_equals(other.diverge, map)

  final def alpha_beta_eta_equals(other: Attrs): Boolean = this.alpha_beta_eta_equals(other, AlphaMapping.Empty)

  def upper: Attrs = Attrs(level.upper, AttrSize.Base, selfUsage.upper, AttrSelfUsage_Erased(), assumptions, AttrDiverge.Base) // todo: check if AttrSelfUsage_Erased is fine

  def erased: Attrs = Attrs(level, size, AttrUsage_Erased(), selfUsage, assumptions, diverge)

  def sized(size: Core): Attrs = Attrs(level, AttrSize_Known(size), usage, selfUsage, assumptions, diverge)

  def unknownFiniteSized: Attrs = Attrs(level, AttrSize_UnknownFinite(), usage, selfUsage, assumptions, diverge)

  def infiniteSized: Attrs = Attrs(level, AttrSize_Infinite(), usage, selfUsage, assumptions, diverge)

  def sizeSucc: Attrs = Attrs(level, size.succ, usage, selfUsage, assumptions, diverge)

  def typeInType: Attrs = Attrs(AttrLevel_UniverseInUniverse(), size, usage, selfUsage, assumptions, diverge)

  def setUsage(x: AttrUsage): Attrs = Attrs(level, size, x, selfUsage, assumptions, diverge)
}

object Attrs {
  val Base = Attrs(AttrLevel.Base, AttrSize.Base, AttrUsage.Base, AttrSelfUsage.Base, AttrAssumptions.Base, AttrDiverge.Base)
}

final case class Type(universe: Core, attrs: Attrs) extends Core {
  override def subst(s: Subst): Type = Type(universe.subst(s), attrs.subst(s))

  override def scan: List[Core] = List(universe) ++ attrs.scan

  override def alpha_beta_eta_equals(other: Core, map: AlphaMapping): Boolean = other match {
    case Type(otherUniverse, otherAttrs) => universe.alpha_beta_eta_equals(otherUniverse, map) && attrs.alpha_beta_eta_equals(otherAttrs, map)
    case _ => false
  }

  def validPlainSubtype(subtype: Type): Boolean = attrs.validPlainSubtype(subtype.attrs)

  def validWeakSubtype(subtype: Type): Boolean = attrs.validWeakSubtype(subtype.attrs)

  def checkPlainSubtype(subtype: Type): Maybe[Unit] = if (this.validPlainSubtype(subtype)) Right(()) else Left(ErrPlainSubtype(this, subtype))

  def checkWeakSubtype(subtype: Type): Maybe[Unit] = if (this.validWeakSubtype(subtype)) Right(()) else Left(ErrWeakSubtype(this, subtype))

  def subsetOrEqual(other: Type): Boolean = universe.alpha_beta_eta_equals(other.universe) && (attrs.alpha_beta_eta_equals(other.attrs) || attrs.merge(other.attrs).alpha_beta_eta_equals(attrs))

  def upperUniverse: Type = Type(Cores.Universe(), attrs.upper)

  def upperKind: Type = Type(Cores.Kind(), attrs.upper)

  def attrsMap(f: Attrs => Attrs): Type = Type(universe, f(attrs))

  override def impl_infer(context: Context): MaybeSt[Type] = for {
    _ <- universe.check(context, Cores.UniverseInfiniteErased)
  } yield upperUniverse

  def erased: Type = Type(universe, attrs.erased)

  def sized(size: Core): Type = Type(universe, attrs.sized(size))

  def typeInType: Type = Type(universe, attrs.typeInType)

  def sizeSucc: Type = Type(universe, attrs.sizeSucc)

  def isErased: Boolean = attrs.usage match {
    case AttrUsage_Erased() => true
    case AttrUsage_Unlimited() | AttrUsage_Once() => false
  }

  def isSelfErased: Boolean = attrs.selfUsage match {
    case AttrSelfUsage_Erased() => true
    case AttrSelfUsage_Unlimited() | AttrSelfUsage_Once() => false
  }
}

object Type {
  def apply(universe: Core, attrs: Attrs) = new Type(universe, attrs)

  def apply(universe: Core) = new Type(universe, Attrs.Base)
}

object Exps {
  final case class Var(x: Identifier) extends ExpNeu {
    def toCoreVar(scope: HashMap[Identifier, VarId]): Cores.Var = scope.get(x) match {
      case Some(v) => Cores.Var(v)
      case None => throw new Error(s"no definition $x")
    }

    override def toCore(scope: HashMap[Identifier, VarId]): Cores.AccessVar = Cores.AccessVar.gen(this.toCoreVar(scope))

    def gen: Cores.Var = Cores.Var(VarId.gen(x))
  }

  final case class Zero() extends Exp {
    override def toCore: Core = Cores.Zero()
  }

  final case class Succ(x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.Succ(x.toCore(scope))
  }

  final case class Nat() extends Exp {
    override def toCore: Core = Cores.Nat()
  }

  final case class Universe() extends Exp {
    override def toCore: Core = Cores.Universe()
  }

  final case class Kind() extends Exp {
    override def toCore: Core = Cores.Kind()
  }

  final case class MakeKind(x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.MakeKind(x.toCore(scope))
  }

  final case class WithAttrSizeFinite(size: Exp, kind: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.WithAttrSizeFinite(size.toCore(scope), kind.toCore(scope))
  }

  final case class WithAttrSizeUnknownFinite(kind: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.WithAttrSizeUnknownFinite(kind.toCore(scope))
  }

  final case class Cons(x: Exp, y: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.Cons(x.toCore(scope), y.toCore(scope))
  }

  final case class Car(x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.Car(x.toCore(scope))
  }

  final case class Cdr(x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.Cdr(x.toCore(scope))
  }

  final case class Sigma(x: Exp, id: Var, y: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = {
      val id0 = id.gen
      Cores.Sigma(x.toCore(scope), id0, y.toCore(scope.updated(id.x, id0.x)))
    }
  }

  final case class Lambda(arg: Var, body: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = {
      val scope0 = scope.updated(arg.x, arg.gen.x)
      Cores.Lambda(arg.toCoreVar(scope0), body.toCore(scope0))
    }
  }

  final case class Pi(x: Exp, id: Var, y: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = {
      val id0 = id.gen
      Cores.Pi(x.toCore(scope), id0, y.toCore(scope.updated(id.x, id0.x)))
    }
  }

  final case class RecPi(x: Exp, id: Var, y: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = {
      val id0 = id.gen
      Cores.RecPi(x.toCore(scope), id0, y.toCore(scope.updated(id.x, id0.x)))
    }
  }

  final case class Rec(id: Var, kind: Exp, x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = {
      val id0 = id.gen
      val scope0 = scope.updated(id.x, id0.x)
      Cores.Rec(id0, kind.toCore(scope0), x.toCore(scope0))
    }
  }

  final case class Recs(bindings: Set[Rec], x: Exp) extends Exp {
    if (bindings.size != bindings.toList.distinctBy(_.id).length) {
      throw new IllegalArgumentException("Recs: duplicate id")
    }

    override def toCore(scope: HashMap[Identifier, VarId]): Core = {
      val scope0 = scope ++ bindings.map(_.id).map(x => (x.x, x.gen.x))
      Cores.Recs(bindings.map(b => Cores.Rec(b.id.toCoreVar(scope0), b.kind.toCore(scope0), b.x.toCore(scope0))), x.toCore(scope0))
    }
  }

  final case class Apply(f: Exp, x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.Apply(f.toCore(scope), x.toCore(scope))
  }

  final case class The(t: Exp, x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.The(t.toCore(scope), x.toCore(scope))
  }

  final case class Quote(x: Symbol) extends Exp {
    override def toCore: Core = Cores.Quote(x)
  }

  final case class Atom() extends Exp {
    override def toCore: Core = Cores.Atom()
  }
}

private def transverse[A](xs: List[Option[A]]): Option[List[A]] = xs match {
  case Nil => Some(Nil)
  case Some(x) :: xs => transverse(xs).map(x :: _)
  case None :: _ => None
}

object Cores {
  final case class Var(x: VarId) {
  }

  // a Var might be checked serval times, so I invited this class for usage checking
  final case class AccessVar(id: Option[UniqueIdentifier], v: Var) extends Core {
    override def scan: List[Core] = List()

    override def subst(s: Subst): Core = s.getOrElse(this.v, this)

    override def impl_infer(context: Context): MaybeSt[Type] = context.getType(v.x) match {
      case Some(t) => for {
        _ <- Recs.checkRecWithoutCheck(context, t, this)
      } yield t
      case None => Left(ErrTypeUnknown(context, this))
    }
  }

  object AccessVar {
    def internal(x: Var): AccessVar = AccessVar(None, x)

    def gen(x: Var): AccessVar = AccessVar(Some(UniqueIdentifier.gen), x)
  }

  private val NatZeroT: Type = Type(Nat())
  private val NatUnknownFiniteErased: Type = NatZeroT.attrsMap(_.unknownFiniteSized).attrsMap(_.erased)

  final case class Zero() extends Core {
    override def scan: List[Core] = List()

    override def subst(s: Subst): Zero = this

    override def impl_infer(context: Context): MaybeSt[Type] = Right(NatZeroT)
  }

  final case class Succ(x: Core) extends Core {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): Succ = Succ(x.subst(s))

    override def impl_infer(context: Context): MaybeSt[Type] = for {
      t <- x.infer(context)
    } yield t.sizeSucc
  }

  private val Universe0Size0: Type = Type(Universe(), Attrs.Base.upper)
  private[core] val UniverseInfiniteErased: Type = Universe0Size0.attrsMap(_.infiniteSized).typeInType.attrsMap(_.erased)
  private val Kind0Size0: Type = Type(Kind(), Attrs.Base.upper)
  private val KindInfiniteErased: Type = Kind0Size0.attrsMap(_.infiniteSized).typeInType.attrsMap(_.erased)

  final case class Nat() extends Core with CoreUniverse {
    override def scan: List[Core] = List()

    override def subst(s: Subst): Nat = this

    override def evalToType(context: Context): MaybeSt[Type] = Right(Type(this))
  }

  // type without attributes
  final case class Universe() extends Core with CoreUniverse {
    override def scan: List[Core] = List()

    override def subst(s: Subst): Universe = this

    override def evalToType(context: Context): MaybeSt[Type] = Right(Universe0Size0)
  }

  // type with attributes
  final case class Kind() extends Core with CoreUniverse {
    override def scan: List[Core] = List()

    override def subst(s: Subst): Kind = this

    override def evalToType(context: Context): MaybeSt[Type] = Right(Kind0Size0)
  }

  final case class MakeKind(x: Core) extends Core with CoreKind {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): MakeKind = MakeKind(x.subst(s))

    override def evalToType(context: Context): MaybeSt[Type] = (x.check(context, UniverseInfiniteErased)).flatMap(_ => {
      Right(Type(x, Attrs.Base))
    })

    override def impl_check(context: Context, t: Type): MaybeSt[Unit] = if (t.universe.alpha_beta_eta_equals(Kind())) {
      x.check(context, Type(Universe(), t.attrs))
    } else {
      Left(ErrCheckFailed(context, t, this, Kind()))
    }
  }

  final case class WithAttrSizeFinite(size: Core, kind: Core) extends Core with CoreKind {
    override def scan: List[Core] = List(size, kind)

    override def subst(s: Subst): WithAttrSizeFinite = WithAttrSizeFinite(size.subst(s), kind.subst(s))

    override def impl_check(context: Context, t: Type): MaybeSt[Unit] = size.check(context, NatUnknownFiniteErased) and kind.check(context, KindInfiniteErased) and kind.check(context, t)

    override def evalToType(context: Context): MaybeSt[Type] = (size.check(context, NatUnknownFiniteErased) and kind.check(context, KindInfiniteErased)).flatMap(_ => {
      kind.evalToType(context).map(_.sized(size))
    })
  }

  final case class WithAttrSizeUnknownFinite(kind: Core) extends Core with CoreKind {
    override def scan: List[Core] = List(kind)

    override def subst(s: Subst): WithAttrSizeUnknownFinite = WithAttrSizeUnknownFinite(kind.subst(s))

    override def impl_check(context: Context, t: Type): MaybeSt[Unit] = kind.check(context, KindInfiniteErased) and kind.check(context, t)

    override def evalToType(context: Context): MaybeSt[Type] = for {
      _ <- kind.check(context, KindInfiniteErased)
      x <- kind.evalToType(context)
    } yield x.attrsMap(_.unknownFiniteSized)
  }

  final case class Cons(x: Core, y: Core) extends Core {
    override def scan: List[Core] = List(x, y)

    override def subst(s: Subst): Cons = Cons(x.subst(s), y.subst(s))

    override def impl_check(context: Context, t: Type): MaybeSt[Unit] = t.universe.reducingMatch(context, {
      case Sigma(a, id, d) => for {
        aT <- a.evalToType(context)
        _ <- t.checkPlainSubtype(aT).l
        _ <- x.check(context, aT)
        innerContext = context.updated(id, aT, x)
        dT <- d.evalToType(innerContext)
        _ <- t.checkPlainSubtype(dT).l
        _ <- y.check(innerContext, dT)
      } yield ()
      case wrong => MaybeStErr(ErrExpected(context, "Sigma", this, wrong))
    })
  }

  final case class Car(x: Core) extends CoreNeu {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): Car = Car(x.subst(s))

    override def impl_infer(context: Context): MaybeSt[Type] = x.infer(context) flatMap {
      case Type(uni, attrs) => uni.reducingMatch(context, {
        case Sigma(a, id, d) => a.evalToType(context)
        case wrong => MaybeStErr(ErrExpected(context, "Sigma", x, wrong))
      })
    }
  }

  final case class Cdr(x: Core) extends CoreNeu {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): Cdr = Cdr(x.subst(s))

    override def impl_infer(context: Context): MaybeSt[Type] = x.infer(context) flatMap {
      case Type(uni, attrs) => uni.reducingMatch(context, {
        case Sigma(a, id, d) => a.evalToType(context).flatMap((at) => d.evalToType(context.updated(id, at)))
        case wrong => MaybeStErr(ErrExpected(context, "Sigma", x, wrong))
      })
    }
  }

  final case class Sigma(x: Core, id: Var, y: Core) extends Core with CoreUniverse {
    override def scan: List[Core] = List(x, y)

    override def subst(s: Subst): Sigma = Sigma(x.subst(s), id, y.subst(s))

    override def evalToType(context: Context): MaybeSt[Type] = Right(Type(this))
  }

  final case class Lambda(arg: Var, body: Core) extends Core {
    override def scan: List[Core] = List(body)

    override def subst(s: Subst): Lambda = Lambda(arg, body.subst(s))

    override def impl_check(context: Context, t: Type): MaybeSt[Unit] = t.universe.reducingMatch(context, {
      case Pi(arg0, id, result0) => for {
        _ <- Recs.checkRecWithoutCheck(context, t, this)
        argT <- arg0.evalToType(context)
        _ <- t.checkWeakSubtype(argT).l
        innerContext = context.updated(arg, argT).updated(id, argT, AccessVar.internal(arg))
        resultT <- result0.evalToType(innerContext)
        _ <- t.checkWeakSubtype(resultT).l
        _ <- body.check(innerContext, resultT)
      } yield ()
      case RecPi(arg0, id, result0) => for {
        _ <- Recs.checkRecWithoutCheck(context, t, this)
        argT <- arg0.evalToType(context)
        _ <- t.checkWeakSubtype(argT).l
        recSize <- (argT.attrs.size match {
          case AttrSize_Known(x) => x.reducingMatch(context, {
            case Succ(x) => MaybeSt.pure(x)
            case _ => MaybeStErr(???)
          })
          case AttrSize_UnknownFinite() | AttrSize_Infinite() => MaybeStErr(???)
        }): MaybeSt[Core]
        innerContext = context.updated(arg, argT).updated(id, argT, AccessVar.internal(arg)).setRecSize(recSize)
        resultT <- result0.evalToType(innerContext)
        _ <- t.checkWeakSubtype(resultT).l
        _ <- body.check(innerContext, resultT)
      } yield ()
      case wrong => MaybeStErr(ErrExpected(context, "Pi", this, wrong))
    })

    def checkWithRecSize(context: Context, t: Type, recSize: Core): Maybe[Unit] = ???
  }

  final case class Pi(x: Core, id: Var, y: Core) extends Core with CoreUniverse {
    override def scan: List[Core] = List(x, y)

    override def subst(s: Subst): Pi = Pi(x.subst(s), id, y.subst(s))

    override def evalToType(context: Context): MaybeSt[Type] = Right(Type(this))
  }

  final case class RecPi(x: Core, id: Var, y: Core) extends Core with CoreUniverse {
    override def scan: List[Core] = List(x, y)

    override def subst(s: Subst): Pi = Pi(x.subst(s), id, y.subst(s))

    override def evalToType(context: Context): MaybeSt[Type] = Right(Type(this))
  }

  final case class Rec(id: Var, kind: Core, x: Core) extends Core {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): Rec = Rec(id, kind.subst(s), x.subst(s))

    private def recs = Recs(Set(this), AccessVar.gen(id))

    override def impl_check(context: Context, t: Type): MaybeSt[Unit] = recs.check(context, t)

    override def impl_infer(context: Context): MaybeSt[Type] = recs.infer(context)

    override def reduce(context: Context): Core = recs.reduce(context)
  }

  final case class Recs(bindings: Set[Rec], x: Core) extends Core {
    if (bindings.size != bindings.toList.distinctBy(_.id).length) {
      throw new IllegalArgumentException("Recs: duplicate id")
    }

    override def reduce(context: Context): Core = ???

    override def scan: List[Core] = bindings.toList ++ List(x)

    override def subst(s: Subst): Recs = Recs(bindings.map(_.subst(s)), x.subst(s))

    private def checkBindings(context: Context): MaybeSt[Context] = {
      val innerContext0 = context.concat(bindings.toList.map(Recs.checkRec(context, _)).map(_.toOption).flatten)

      def step(stepContext: Context) = context.concat(bindings.toList.map(Recs.checkRec(stepContext, _)).map(_.toOption).flatten)

      val innerContext = step(step(step(step(step(step(step(step(step(step(step(step(step(step(step(step(innerContext0))))))))))))))))

      for {
        addition <- bindings.toList.map(Recs.checkRec(innerContext, _)).traverse
      } yield context.concat(addition)
    }

    override def impl_check(context: Context, t: Type): MaybeSt[Unit] = for {
      ctx <- checkBindings(context)
      _ <- x.check(ctx, t)
    } yield ()

    override def impl_infer(context: Context): MaybeSt[Type] = for {
      ctx <- checkBindings(context)
      t <- x.infer(ctx)
    } yield t
  }

  object Recs {
    private def extract(context: Context, x: Core): List[Core] = x match {
      case v: AccessVar => context.getValue(v.v).toList
      case other => other.scan
    }

    private def coreContains(context: Context, element: Core, history: Set[Core], current: Core): Boolean = {
      if (element == current) {
        true
      } else if (history.contains(current)) {
        false
      } else {
        val newHistory = history.incl(current)
        extract(context, current).exists(coreContains(context, element, newHistory, _))
      }
    }

    private def isRecursive(context: Context, x: Core): Boolean = extract(context, x).exists(coreContains(context, x, Set(), _))

    private def checkRec(context: Context, rec: Rec): MaybeSt[(VarId, Type, Core)] = for {
      kind <- rec.kind.evalToType(context)
      //_ <- checkRec(context, kind, rec.x) // will be checker in other parts
    } yield (rec.id.x, kind, rec.x)

    def checkRecWithoutCheck(context: Context, kind: Type, x: Core): MaybeSt[Unit] = if (isRecursive(context, x)) {
      if (kind.attrs.size == AttrSize_UnknownFinite()) {
        MaybeStErr(ErrUnknownFiniteRec(context, x, kind))
        // other parts will handle finite and infinite correctly
      } else {
        kind.universe.weakHeadNormalForm(context) match {
          case Pi(arg, argId, result) => for {
            argK <- arg.evalToType(context)
            resultK <- result.evalToType(context.updated(argId, argK))
            _ <- if (resultK.attrs.diverge == AttrDiverge_Yes()) Right(()) else Left(ErrNotDivergePiRec(context, x, kind))
          } yield ()
          case _: CoreNeu => MaybeStErr(ErrUnknownTypeRec(context, x, kind))
          case _: RecPi => MaybeSt.pure(())
          case _ => MaybeSt.pure(())
        }
      }
    } else {
      MaybeSt.pure(())
    }

    def checkRec(context: Context, kind: Type, x: Core): MaybeSt[Unit] = for {
      _ <- x.check(context, kind)
      _ <- checkRecWithoutCheck(context, kind, x)
    } yield ()
  }

  final case class Apply(f: Core, x: Core) extends CoreNeu {
    override def scan: List[Core] = List(f, x)

    override def subst(s: Subst): Apply = Apply(f.subst(s), x.subst(s))

    override def reduce(context: Context): Core = f.infer(context).flatMap(t => t.universe.reducingMatch(context, {
      case Pi(argT, tid, resultT) => f.reducingMatch(context, {
        case Lambda(arg, body) => argT.evalToType(context).flatMap(argK => Right(body.subst(arg, argK, x)))
        case wrong => MaybeStErr(ErrExpectedV(context, "Pi", wrong))
      })
      case RecPi(argT, tid, resultT) => f.reducingMatch(context, {
        case Lambda(arg, body) => argT.evalToType(context).flatMap(argK => {
          val argK0 = context.getRecSize match {
            case None => argK
            case Some(size) => argK.attrsMap(_.sized(size))
          }
          MaybeSt.pure(body.subst(arg, argK0, x))
        })
        case wrong => MaybeStErr(ErrExpectedV(context, "Pi", wrong))
      })
      case wrong => MaybeStErr(ErrExpected(context, "Pi", f, wrong))
    })) getOrElse this

    override def impl_infer(context: Context): MaybeSt[Type] = f.infer(context).flatMap(t => t.universe.reducingMatch(context, {
      case Pi(argT, tid, resultT) => for {
        argK <- argT.evalToType(context)
        _ <- x.check(context, argK)
        resultK <- resultT.evalToType(context.updated(tid, argK, x))
      } yield resultK
      case RecPi(argT, tid, resultT) => for {
        argK <- argT.evalToType(context)
        argK0 = context.getRecSize match {
          case None => argK
          case Some(size) => argK.attrsMap(_.sized(size))
        }
        _ <- x.check(context, argK)
        resultK <- resultT.evalToType(context.updated(tid, argK, x))
      } yield resultK
      case wrong => MaybeStErr(ErrExpected(context, "Pi", f, wrong))
    }))
  }

  final case class The(t: Core, x: Core) extends Core {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): The = The(t.subst(s), x.subst(s))

    override def impl_infer(context: Context): MaybeSt[Type] = for {
      result <- t.evalToType(context)
      _ <- x.check(context, result)
    } yield result

    override def reduce(context: Context): Core = t.evalToType(context).map(InternalThe(_, x)) getOrElse this
  }

  final case class InternalThe(t: Type, x: Core) extends Core {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): InternalThe = InternalThe(t.subst(s), x.subst(s))

    override def impl_infer(context: Context): MaybeSt[Type] = for {
      _ <- x.check(context, t)
    } yield t

    override def reduce(context: Context): Core = x // x.check(context, t).map(_ => x) getOrElse this
  }

  private val AtomT = Type(Atom())

  final case class Quote(x: Symbol) extends Core {
    override def scan: List[Core] = List()

    override def subst(s: Subst): Quote = this

    override def impl_infer(context: Context): MaybeSt[Type] = Right(AtomT)
  }

  final case class Atom() extends Core with CoreUniverse {
    override def scan: List[Core] = List()

    override def subst(s: Subst): Atom = this

    override def evalToType(context: Context): MaybeSt[Type] = Right(Type(this))
  }

}