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
      assert(infer("(the (withAttrSizeUnknownFinite (makeKind nat)) (succ (succ zero)))").isRight)
      assert(infer("(the (withAttrSizeFinite zero (makeKind nat)) (succ (succ zero)))").isLeft)
      assert(infer("(the (withAttrSizeFinite (succ (succ zero)) (makeKind nat)) (succ (succ zero)))").isRight)
      assert(infer("(the (withAttrSizeFinite zero (makeKind nat)) zero)").isRight)
      assert(infer("(the (withAttrSizeUnknownFinite (makeKind nat)) 'a)").isLeft)
      assert(infer("(the (makeKind nat) (succ (succ zero)))").isLeft)
      assert(infer("(the (makeKind (pi (withAttrSizeUnknownFinite (makeKind nat)) $_ (withAttrSizeUnknownFinite (makeKind nat)))) (lambda $x $x))").isRight)
      assert(infer("(apply (the (makeKind (pi (withAttrSizeUnknownFinite (makeKind nat)) $_ (withAttrSizeUnknownFinite (makeKind nat)))) (lambda $x $x)) zero)").isRight)
      assert(infer("(the (withAttrSizeFinite zero (makeKind (sigma (makeKind nat) $_ (makeKind nat)))) (cons zero zero))").isLeft)
      assert(infer("(the (withAttrSizeFinite (succ zero) (makeKind (sigma (makeKind nat) $_ (makeKind nat)))) (cons zero zero))").isRight)
    }
  }
}