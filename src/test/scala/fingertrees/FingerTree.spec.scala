package fingertrees

import org.specs2.mutable._

class FingerTreeSpec extends Specification {
	private implicit def branchToIterable[A](branch: Branch[A, ?]): Iterable[A] = branch match {
		case Branch2(value1, value2, _) => Iterable(value1, value2)
		case Branch3(value1, value2, value3, _) => Iterable(value1, value2, value3)
	}

	private def digitToVector[A](digit: Digit[A]): Vector[A] = digit match {
		case Digit1(value1) => Vector(value1)
		case Digit2(value1, value2) => Vector(value1, value2)
		case Digit3(value1, value2, value3) => Vector(value1, value2, value3)
		case Digit4(value1, value2, value3, value4) => Vector(value1, value2, value3, value4)
	}

	private def fingerTreeListToVector[A](fingerTreeList: FingerTreeList[A]): Vector[A] =
		fingerTreeList.underlying match {
			case FingerTreeEmpty() => Vector.empty
			case FingerTreeSingle(value) => Vector(value)
			case FingerTreeDeep(prefix, nextLayer, suffix, _) =>
				digitToVector(prefix) ++
					fingerTreeListToVector(new FingerTreeList(nextLayer)).flatten ++
					digitToVector(suffix)
		}

	"FingerTreeList" should {
		"appending should work" in {
			def generateList(length: Int): FingerTreeList[Int] = if (length == 0) {
				FingerTreeList.empty
			} else {
				generateList(length - 1).append(length - 1)
			}

			/*
			 * We want to test converting a `FingerTreeEmpty` into a `FingerTreeSingle`, then into a
			 * `FingerTreeDeep`, then into a `FingerTreeDeep` with a full suffix and a nonempty
			 * next layer.
			 */
			fingerTreeListToVector(generateList(0)) must beEmpty
			fingerTreeListToVector(generateList(1)) mustEqual Vector(0)
			fingerTreeListToVector(generateList(2)) mustEqual Vector(0, 1)
			fingerTreeListToVector(generateList(3)) mustEqual Vector(0, 1, 2)
			fingerTreeListToVector(generateList(4)) mustEqual Vector(0, 1, 2, 3)
			fingerTreeListToVector(generateList(5)) mustEqual Vector(0, 1, 2, 3, 4)
			fingerTreeListToVector(generateList(6)) mustEqual Vector(0, 1, 2, 3, 4, 5)
		}

		"concatenate should work" in {
			val emptyList = FingerTreeList.empty
			val oneElementList = FingerTreeList(0)
			val twoElementList = FingerTreeList(0, 1)
			val tenElementList = FingerTreeList(Range(0, 10): _*)

			emptyList.concatenate(emptyList) mustEqual emptyList
			emptyList.concatenate(oneElementList) mustEqual oneElementList
			emptyList.concatenate(oneElementList) mustEqual oneElementList
			emptyList.concatenate(tenElementList) mustEqual tenElementList

			oneElementList.concatenate(emptyList) mustEqual oneElementList
			oneElementList.concatenate(oneElementList) mustEqual FingerTreeList(0, 0)
			oneElementList.concatenate(twoElementList) mustEqual FingerTreeList(0, 0, 1)
			oneElementList.concatenate(tenElementList) mustEqual
				FingerTreeList(Seq(0) ++ Range(0, 10): _*)

			twoElementList.concatenate(emptyList) mustEqual twoElementList
			twoElementList.concatenate(oneElementList) mustEqual FingerTreeList(0, 1, 0)
			twoElementList.concatenate(twoElementList) mustEqual FingerTreeList(0, 1, 0, 1)
			twoElementList.concatenate(tenElementList) mustEqual
				FingerTreeList(Seq(0, 1) ++ Range(0, 10): _*)

			tenElementList.concatenate(emptyList) mustEqual tenElementList
			tenElementList.concatenate(oneElementList) mustEqual
				FingerTreeList(Range(0, 10) ++ Seq(0): _*)

			tenElementList.concatenate(twoElementList) mustEqual
				FingerTreeList(Range(0, 10) ++ Seq(0, 1): _*)

			tenElementList.concatenate(tenElementList) mustEqual
				FingerTreeList(Range(0, 10) ++ Range(0, 10): _*)
		}

		"get should return None on empty lists" in {
			FingerTreeList.empty.get(0) must beNone
		}

		"get should return None on invalid indices" in {
			FingerTreeList.empty.get(0) must beNone

			val list = FingerTreeList(0, 1)

			list.get(-1) must beNone
			list.get(2) must beNone
		}

		"get should return the correct element" in {
			FingerTreeList(0).get(0) must beSome(0)
			FingerTreeList(0, 1).get(1) must beSome(1)
			FingerTreeList(Range(0, 10): _*).get(5) must beSome(5)
		}

		"length should return the correct length" in {
			FingerTreeList.empty.length mustEqual 0
			FingerTreeList(0).length mustEqual 1
			FingerTreeList(0, 1).length mustEqual 2
			FingerTreeList(Range(0, 10): _*).length mustEqual 10
		}

		"popLeft should return the list's head and tail" in {
			FingerTreeList.empty.popLeft must beNone
			FingerTreeList(0).popLeft must beSome((0, FingerTreeList.empty))
			FingerTreeList(0, 1).popLeft must beSome((0, FingerTreeList(1)))
			FingerTreeList(Range(0, 10): _*).popLeft must
				beSome((0, FingerTreeList(Range(1, 10): _*)))
		}

		"popRight should return the list's init and last" in {
			FingerTreeList.empty.popRight must beNone
			FingerTreeList(0).popRight must beSome((FingerTreeList.empty, 0))
			FingerTreeList(0, 1).popRight must beSome((FingerTreeList(0), 1))
			FingerTreeList(Range(0, 10): _*).popRight must
				beSome((FingerTreeList(Range(0, 9): _*), 9))
		}

		"prepending should work" in {
			def generateList(end: Int, start: Int = 0): FingerTreeList[Int] = if (start == end) {
				FingerTreeList.empty
			} else {
				generateList(end, start + 1).prepend(start)
			}

			/*
			 * We want to test converting a `FingerTreeEmpty` into a `FingerTreeSingle`, then into a
			 * `FingerTreeDeep`, then into a `FingerTreeDeep` with a full suffix and a nonempty
			 * next layer.
			 */
			fingerTreeListToVector(generateList(0)) must beEmpty
			fingerTreeListToVector(generateList(1)) mustEqual Vector(0)
			fingerTreeListToVector(generateList(2)) mustEqual Vector(0, 1)
			fingerTreeListToVector(generateList(3)) mustEqual Vector(0, 1, 2)
			fingerTreeListToVector(generateList(4)) mustEqual Vector(0, 1, 2, 3)
			fingerTreeListToVector(generateList(5)) mustEqual Vector(0, 1, 2, 3, 4)
			fingerTreeListToVector(generateList(6)) mustEqual Vector(0, 1, 2, 3, 4, 5)
		}
	}
}
