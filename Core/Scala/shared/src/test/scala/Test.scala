import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.funspec.AnyFunSpec
import schnappi.core._

def infer(x:String) = parseThrows(x).toCore.infer

final class Test extends AnyFunSpec {
  describe("basic") {
    it("works") {
      assert(infer("(succ zero)").isRight)
      assert(infer("'a").isRight)
      //assert(infer("(the (makeKind nat) (succ (succ zero)))").isRight)
    }
  }
}