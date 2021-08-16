package schnappi.full
import schnappi.core
import schnappi.core.Err
import schnappi.core.Identifier
import schnappi.core.UniqueIdentifier
import scala.collection.immutable.HashMap

sealed trait Exp {

}

final case class Context() // todo

type UnifySubstitution = HashMap[Hole, Core]

type State = UnifySubstitution

final case class ElabOk[T](st: State, x: T)

type Elab[T] = State=> Option[ElabOk[T]]

sealed trait Core {
  def check(context: Context, t: Core): Elab[core.Core] = ???
  def infer(context:Context): Elab[(Core, core.Core)] = ???
  def unify(other: Core): Option[UnifySubstitution] = ???
  def walk: Elab[Core] = ???
}
final case class Hole(id: Identifier, uid: UniqueIdentifier) extends Core