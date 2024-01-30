package fingertrees

import Monoid.InfixFold

/**
  * A [[Measurer]] produces annotations for inclusion in a [[Branch]] or [[FingerTree]].
  * Many methods on `FingerTree[Element, Annotation]` require an implicit
  * `Measurer[Element, Annotation]` to produce annotations for the new `FingerTree`s they generate.
  *
  * @tparam Element The type of object for which an annotation will be produced.
  * @tparam Annotation The type of annotation to produce.
  *
  * @param annotationMonoid A `Monoid[Annotation]` is bundled for convenience so new [[Measurer]]s
  * can be produced from this one and so monoidal addition can be easily performed on the resulting
  * annotations.
  */
abstract class Measurer[-Element, Annotation](implicit val annotationMonoid: Monoid[Annotation]) {
	self =>
		def measure(element: Element): Annotation
		def toBranchMeasurer: Measurer[Branch[Element, Annotation], Annotation] =
			new Measurer[Branch[Element, Annotation], Annotation]()(annotationMonoid) {
				override def measure(branch: Branch[Element, Annotation]): Annotation =
					branch.annotation
			}

		def toDigitMeasurer: Measurer[Digit[Element], Annotation] =
			new Measurer[Digit[Element], Annotation]()(annotationMonoid) {
				override def measure(digit: Digit[Element]): Annotation = digit match {
					case Digit1(value1) => self.measure(value1)
					case Digit2(value1, value2) => self.measure(value1) + self.measure(value2)
					case Digit3(value1, value2, value3) =>
						self.measure(value1) + self.measure(value2) + self.measure(value3)

					case Digit4(value1, value2, value3, value4) =>
						self.measure(value1) +
						self.measure(value2) +
						self.measure(value3) +
						self.measure(value4)
				}
			}

		def toListMeasurer: Measurer[FingerTree[Element, Annotation], Annotation] =
			new Measurer[FingerTree[Element, Annotation], Annotation]()(annotationMonoid) {
				override def measure(list: FingerTree[Element, Annotation]): Annotation =
					list match {
						case FingerTreeEmpty() => implicitly[Monoid[Annotation]].empty
						case FingerTreeSingle(value) => self.measure(value)
						case FingerTreeDeep(_, _, _, annotation) => annotation
					}
			}
}

/**
  * A data structure encapsulating the monoidal identity value and addition operation for some type
  * `A`.
  *
  * See
  * [[https://en.wikipedia.org/wiki/Monoid#Monoids_in_computer_science Monoids in computer science]]
  * for more information.
  */
trait Monoid[A] {
	val empty: A

	def fold(value1: A, value2: A): A
}

object Monoid {
	implicit class InfixFold[A](value: A)(implicit valueMonoid: Monoid[A]) {
		def +(other: A): A = valueMonoid.fold(value, other)
	}

	implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
		override val empty: Int = 0
		override def fold(value1: Int, value2: Int) = value1 + value2
	}
}

/**
  * Stores the left, center, and right components produced as the result of a [[FingerTree]] split
  * operation, which forms the backbone for random-access in [[FingerTreeList]].
  *
  * @see See [[FingerTree.splitByWithInitial]] for more information.
  */
case class SplitPoint[+Element, Annotation](
	left: FingerTree[Element, Annotation],
	center: Element,
	right: FingerTree[Element, Annotation]
)

sealed trait Branch[+Element, Annotation] { self =>
	val annotation: Annotation

	def toDigit: Digit[Element] = self match {
		case Branch2(value1, value2, _) => Digit2(value1, value2)
		case Branch3(value1, value2, value3, _) => Digit3(value1, value2, value3)
	}

	def toList: FingerTree[Element, Annotation] = self match {
		case Branch2(value1, value2, annotation) =>
			FingerTreeDeep(Digit1(value1), FingerTreeEmpty(), Digit1(value2), annotation)

		case Branch3(value1, value2, value3, annotation) =>
			FingerTreeDeep(Digit1(value1), FingerTreeEmpty(), Digit2(value2, value3), annotation)
	}
}

case class Branch2[+Element, Annotation](value1: Element, value2: Element, annotation: Annotation)
	extends Branch[Element, Annotation]

object Branch2 {
	def apply[Element, Annotation](value1: Element, value2: Element)(implicit
		measurer: Measurer[Element, Annotation]
	) = {
		import measurer.annotationMonoid

		new Branch2(value1, value2, measurer.measure(value1) + measurer.measure(value2))
	}
}

case class Branch3[+Element, Annotation](
	value1: Element,
	value2: Element,
	value3: Element,
	annotation: Annotation
) extends Branch[Element, Annotation]

object Branch3 {
	def apply[Element, Annotation](value1: Element, value2: Element, value3: Element)(implicit
		measurer: Measurer[Element, Annotation]
	) = {
		import measurer.annotationMonoid

		new Branch3(
			value1,
			value2,
			value3,
			measurer.measure(value1) + measurer.measure(value2) + measurer.measure(value3)
		)
	}
}

sealed trait Digit[+Element] { self =>
	val value1: Element

	def append[Other >: Element](other: Other): Option[Digit[Other]] = self match {
		case Digit1(value1) => Some(Digit2(value1, other))
		case Digit2(value1, value2) => Some(Digit3(value1, value2, other))
		case Digit3(value1, value2, value3) => Some(Digit4(value1, value2, value3, other))
		case _: Digit4[Element] => None
	}

	def popLeft: Option[(Element, Digit[Element])] = self match {
		case _: Digit1[Element] => None
		case Digit2(value1, value2) => Some((value1, Digit1(value2)))
		case Digit3(value1, value2, value3) => Some((value1, Digit2(value2, value3)))
		case Digit4(value1, value2, value3, value4) =>
			Some((value1, Digit3(value2, value3, value4)))
	}

	def popRight: Option[(Digit[Element], Element)] = self match {
		case _: Digit1[Element] => None
		case Digit2(value1, value2) => Some((Digit1(value1), value2))
		case Digit3(value1, value2, value3) => Some((Digit2(value1, value2), value3))
		case Digit4(value1, value2, value3, value4) =>
			Some((Digit3(value1, value2, value3), value4))
	}

	def prepend[Other >: Element](other: Other): Option[Digit[Other]] = self match {
		case Digit1(value1) => Some(Digit2(other, value1))
		case Digit2(value1, value2) => Some(Digit3(other, value1, value2))
		case Digit3(value1, value2, value3) => Some(Digit4(other, value1, value2, value3))
		case _: Digit4[Element] => None
	}

	def toList[Annotation](implicit
		measurer: Measurer[Element, Annotation]
	): FingerTree[Element, Annotation] = {
		lazy val annotation = measurer.toDigitMeasurer.measure(self)

		self match {
			case Digit1(value1) => FingerTreeSingle(value1)
			case Digit2(value1, value2) =>
				FingerTreeDeep(Digit1(value1), FingerTreeEmpty(), Digit1(value2), annotation)

			case Digit3(value1, value2, value3) =>
				FingerTreeDeep(
					Digit2(value1, value2),
					FingerTreeEmpty(),
					Digit1(value3),
					annotation
				)

			case Digit4(value1, value2, value3, value4) =>
				FingerTreeDeep(
					Digit3(value1, value2, value3),
					FingerTreeEmpty(),
					Digit1(value4),
					annotation
				)
		}
	}
}

case class Digit1[+A](value1: A) extends Digit[A]
case class Digit2[+A](value1: A, value2: A) extends Digit[A]
case class Digit3[+A](value1: A, value2: A, value3: A) extends Digit[A]
case class Digit4[+A](value1: A, value2: A, value3: A, value4: A) extends Digit[A]

/**
  * From [[https://en.wikipedia.org/wiki/Finger_tree Finger tree]]:
  *
  * "In computer science, a finger tree is a purely functional data structure that can be used to
  * efficiently implement other functional data structures. A finger tree gives
  * amortized constant time access to the 'fingers' (leaves) of the tree, which is where data is
  * stored, and concatenation and splitting logarithmic time in the size of the smaller piece. It
  * also stores in each internal node the result of applying some associative operation to its
  * descendants.
  *
  * This 'summary' data stored in the internal nodes can be used to provide the functionality of
  * data structures other than trees."
  *
  * Our finger tree implementation is inspired by
  * [[https://andrew.gibiansky.com/blog/haskell/finger-trees/ Finger Trees]] and supports the
  * following foundational operations.
  *
  * | Operation      | Amortized time complexity | Worst-case time complexity |
  * | ---            | ---                       | ---                        |
  * | append         | O(1)                      | O(log n)                   |
  * | concatenate    | O(1)                      | O(log min(n, m))           |
  * | popLeft        | O(1)                      | O(log n)                   |
  * | popRight       | O(1)                      | O(log n)                   |
  * | prepend        | O(1)                      | O(log n)                   |
  * | splitByInitial | O(1)                      | O(log n)                   |
  *
  * Within our implementation, the "'summary' data" referenced by the Wikipedia description are
  * referred to as annotations. Each annotation is attached to a sub-tree of the broader
  * [[FingerTree]], and, in the context of a [[FingerTree]]-backed sequential data structure, can be
  * thought of as the cached result of some computation applied on that sub-tree's
  * respective subsequence of the broader tree.
  *
  * This may make more sense when [[FingerTree]]s are used as sequences, because then, that
  * computation will usually be the sub-tree's length, in which case that sub-tree will be annotated
  * with the number of elements it contains.
  *
  * This is useful because [[FingerTree]]s support an amortized constant-time [[splitByWithInitial]]
  * operation allowing one to search the [[FingerTree]] by annotation, splitting it into two where a
  * match has been found; this forms the backbone for random access. See [[splitByWithInitial]] for
  * more information on that.
  *
  * NOTE:
  *
  * [[FingerTree]] assumes that the aforementioned computation is monotonically increasing
  * over the tree's elements
  * (see [[https://en.wikipedia.org/wiki/Monotonic_function Monotonic function]])—that is, that the
  * result of applying the computation over the tree is equal to the monoidal sum of applying the
  * computation to each of its sub-trees.
  *
  * TL;DR: a tree's length should be equal to the sum of its sub-trees' lengths.
  */
sealed trait FingerTree[+Element, Annotation] { self =>
	def append[Other >: Element](other: Other)(implicit
		elementMeasurer: Measurer[Other, Annotation]
	): FingerTree[Other, Annotation] = self match {
		case FingerTreeEmpty() => FingerTreeSingle(other)
		case FingerTreeSingle(value) =>
			FingerTreeDeep(Digit1(value), FingerTreeEmpty[Annotation](), Digit1(other))

		case FingerTreeDeep(prefix, nextLayer, suffix, _) =>
			implicit val branchMeasurer: Measurer[Branch[Other, Annotation], Annotation] =
				elementMeasurer.toBranchMeasurer

			suffix
				.append(other)
				.map(FingerTreeDeep(prefix, nextLayer, _))
				.getOrElse(
					suffix match {
						case Digit4(value1, value2, value3, value4) =>
							FingerTreeDeep(
								prefix,
								nextLayer.append(Branch3(value1, value2, value3)),
								Digit2(value4, other)
							)

						case _ => throw new MatchError(s"Should be Digit4: $prefix")
					}
				)
	}

	def concatenate[Other >: Element](
		other: FingerTree[Other, Annotation]
	)(implicit measurer: Measurer[Other, Annotation]): FingerTree[Other, Annotation] =
		FingerTree.concatenateThree(this, FingerTreeEmpty(), other)

	/**
	  * Return the first element of the [[FingerTree]] if it isn't empty.
	  */
	def head(implicit measurer: Measurer[Element, Annotation]): Option[Element] =
		popLeft.map { case (head, _) => head }

	/**
	  * Return every element of the [[FingerTree]] but its last, if it isn't empty.
	  */
	def init(implicit
		measurer: Measurer[Element, Annotation]
	): Option[FingerTree[Element, Annotation]] = popRight.map { case (init, _) => init }

	/**
	  * Return the last element of the [[FingerTree]] if it isn't empty.
	  */
	def last(implicit measurer: Measurer[Element, Annotation]): Option[Element] =
		popRight.map { case (_, last) => last }

	/**
	  * If the [[FingerTree]] isn't empty, return its first element and remaining elements after
	  * removing the first element.
	  */
	def popLeft(implicit
		measurer: Measurer[Element, Annotation]
	): Option[(Element, FingerTree[Element, Annotation])] = self match {
		case FingerTreeEmpty() => None
		case FingerTreeSingle(value) => Some((value, FingerTreeEmpty()))
		case self: FingerTreeDeep[Element, Annotation] =>
			Some(
				self
					.popLeftFromPrefix
					.orElse(self.popLeftFromNextLayer)
					.getOrElse(self.popLeftFromSuffix)
			)
	}

	/**
	  * If the [[FingerTree]] isn't empty, return every element but its last, and its last element.
	  */
	def popRight(implicit
		measurer: Measurer[Element, Annotation]
	): Option[(FingerTree[Element, Annotation], Element)] = self match {
		case FingerTreeEmpty() => None
		case FingerTreeSingle(value) => Some((FingerTreeEmpty(), value))
		case self: FingerTreeDeep[Element, Annotation] =>
			Some(
				self
					.popRightFromSuffix
					.orElse(self.popRightFromNextLayer)
					.getOrElse(self.popRightFromPrefix)
			)
	}

	def prepend[Other >: Element](other: Other)(implicit
		elementMeasurer: Measurer[Other, Annotation]
	): FingerTree[Other, Annotation] = self match {
		case FingerTreeEmpty() => FingerTreeSingle(other)
		case FingerTreeSingle(value) =>
			FingerTreeDeep(Digit1(other), FingerTreeEmpty[Annotation](), Digit1(value))

		case FingerTreeDeep(prefix, nextLayer, suffix, _) =>
			implicit val branchMeasurer: Measurer[Branch[Other, Annotation], Annotation] =
				elementMeasurer.toBranchMeasurer

			prefix.prepend(other).map(FingerTreeDeep(_, nextLayer, suffix)).getOrElse(
				prefix match {
					case Digit4(value1, value2, value3, value4) =>
						FingerTreeDeep(
							Digit2(other, value1),
							nextLayer.prepend(Branch3(value2, value3, value4)),
							suffix
						)

					case _ => throw new MatchError(s"Expected $prefix to be a Digit4.")
				}
			)
	}

	/**
	  * [[splitByWithInitial]] treats the [[FingerTree]] as a sequential data structure, searching
	  * for the first point (from left to right) where `predicate` returns `true`. It does this in
	  * amortized constant-time, which forms the backbone for random access.
	  *
	  * Because annotations are required to be
	  * [[https://en.wikipedia.org/wiki/Monoid#Monoids_in_computer_science monoidal]] as per
	  * [[Measurer]], they have an identity value and associative monoidal addition operator.
	  *
	  * Effectively (read: not actually), [[splitByWithInitial]] begins with the annotation identity
	  * and monoidally adds each element's annotation, stopping when `predicate` returns `true` over
	  * the current annotation.
	  *
	  * [[splitByWithInitial]] returns a [[SplitPoint]] containing:
	  * - Those elements to the left of that on which it stopped
	  * - The element on which it stopped itself
	  * - Those elements to the right of that on which it stopped
	  */
	def splitByWithInitial(predicate: Annotation => Boolean)(initialAnnotation: Annotation)(
		implicit elementMeasurer: Measurer[Element, Annotation]
	): Option[SplitPoint[Element, Annotation]] = {
		import elementMeasurer.annotationMonoid

		self match {
			case FingerTreeEmpty() => None
			case FingerTreeSingle(value) =>
				Option.when(predicate(initialAnnotation + elementMeasurer.measure(value)))(
					SplitPoint(FingerTreeEmpty(), value, FingerTreeEmpty())
				)

			case list @ FingerTreeDeep(prefix, nextLayer, _, annotation) =>
				if (!predicate(initialAnnotation + annotation)) {
					None
				} else {
					val digitMeasurer = elementMeasurer.toDigitMeasurer
					val branchMeasurer = elementMeasurer.toBranchMeasurer
					val nextLayerMeasurer = branchMeasurer.toListMeasurer

					list
						.splitByInPrefix(predicate)(initialAnnotation)
						.orElse(
							list.splitByInNextLayer(predicate)(
								initialAnnotation + digitMeasurer.measure(prefix)
							)
						)
						.orElse(
							list.splitByInSuffix(predicate)(
								initialAnnotation +
								digitMeasurer.measure(prefix) +
								nextLayerMeasurer.measure(nextLayer)
							)
						)
				}
		}
	}

	/**
	  * Return every element of the [[FingerTree]] but its first, if it isn't empty.
	  */
	def tail(implicit
		measurer: Measurer[Element, Annotation]
	): Option[FingerTree[Element, Annotation]] = popLeft.map { case (_, tail) => tail }

	def toDigit(implicit measurer: Measurer[Element, Annotation]): Option[Digit[Element]] =
		self match {
			case FingerTreeSingle(value) => Some(Digit1(value))
			case FingerTreeDeep(Digit1(value1), FingerTreeEmpty(), Digit2(value2, value3), _) =>
				Some(Digit3(value1, value2, value3))

			case FingerTreeDeep(
				Digit1(value1),
				FingerTreeEmpty(),
				Digit3(value2, value3, value4),
				_
			) => Some(Digit4(value1, value2, value3, value4))

			case FingerTreeDeep(
				Digit1(value1),
				FingerTreeSingle(Branch2(value2, value3, _)),
				Digit1(value4),
				_
			) => Some(Digit4(value1, value2, value3, value4))

			case FingerTreeDeep(Digit2(value1, value2), FingerTreeEmpty(), Digit1(value3), _) =>
				Some(Digit3(value1, value2, value3))

			case FingerTreeDeep(
				Digit3(value1, value2, value3),
				FingerTreeEmpty(),
				Digit1(value4),
				_
			) => Some(Digit4(value1, value2, value3, value4))

			case _ => None
		}

	/**
	  * [[toBranchList]] compresses the [[FingerTree]], combining nodes into [[Branch]]s that the
	  * resulting [[FingerTree]] will contain. For example, the following [[FingerTree]]s would be
	  * transformed like so.
	  *
	  * [1, 2] -> [Branch2(1, 2)]
	  * [1, 2, 3] -> [Branch3(1, 2, 3)]
	  * [1, 2, 3, 4] -> [Branch2(1, 2), Branch2(3, 4)]
	  * [1, 2, 3, 4, 5] -> [Branch(1, 2), Branch3(3, 4, 5)]
	  *
	  * This is useful when generating the `nextLayer` of a [[FingerTreeDeep]].
	  */
	@throws[MatchError]("if the finger tree has less than two elements")
	def toBranchList(implicit
		elementMeasurer: Measurer[Element, Annotation]
	): FingerTree[Branch[Element, Annotation], Annotation] = self match {
		case self @ FingerTreeEmpty() => self
		case FingerTreeDeep(Digit1(value1), FingerTreeEmpty(), Digit1(value2), _) =>
			FingerTreeSingle(Branch2(value1, value2))

		case FingerTreeDeep(Digit1(value1), FingerTreeEmpty(), Digit2(value2, value3), _) =>
			FingerTreeSingle(Branch3(value1, value2, value3))

		case FingerTreeDeep(Digit2(value1, value2), FingerTreeEmpty(), Digit1(value3), _) =>
			FingerTreeSingle(Branch3(value1, value2, value3))

		case _ =>
			implicit val branchMeasurer: Measurer[Branch[Element, Annotation], Annotation] =
				elementMeasurer.toBranchMeasurer

			popLeft
				.flatMap { case (first, tail1) =>
					tail1.popLeft.map { case (second, tail2) => (first, second, tail2) }
				}
				.map { case (first, second, tail) =>
					tail.toBranchList.prepend(Branch2(first, second))
				}
				.getOrElse(throw new MatchError(s"Expected $self to have at least two elements."))
	}

}

object FingerTree {
	def concatenateThree[Element, Annotation](
		list1: FingerTree[Element, Annotation],
		list2: FingerTree[Element, Annotation],
		list3: FingerTree[Element, Annotation]
	)(implicit elementMeasurer: Measurer[Element, Annotation]): FingerTree[Element, Annotation] =
		(list1, list3) match {
			case (FingerTreeEmpty(), _) =>
				list2
					.popLeft
					.map { case (head, tail) =>
						concatenateThree(FingerTreeEmpty(), tail, list3).prepend(head)
					}
					.getOrElse(list3)

			case (FingerTreeSingle(value), _) =>
				concatenateThree(FingerTreeEmpty(), list2, list3).prepend(value)

			case (_, FingerTreeEmpty()) =>
				list2
					.popRight
					.map { case (init, last) =>
						concatenateThree(list1, init, FingerTreeEmpty()).append(last)
					}
					.getOrElse(list1)

			case (_, FingerTreeSingle(value)) =>
				concatenateThree(list1, list2, FingerTreeEmpty()).append(value)

			case (
				FingerTreeDeep(prefix1, nextLayer1, suffix1, _),
				FingerTreeDeep(prefix3, nextLayer3, suffix3, _)
			) =>
				implicit val branchMeasurer: Measurer[Branch[Element, Annotation], Annotation] =
					elementMeasurer.toBranchMeasurer

				val nextLayerMiddle =
					concatenateThree(suffix1.toList, list2, prefix3.toList).toBranchList

				val nextLayer = concatenateThree(nextLayer1, nextLayerMiddle, nextLayer3)

				FingerTreeDeep(prefix1, nextLayer, suffix3)
		}
}

case class FingerTreeEmpty[Annotation]() extends FingerTree[Nothing, Annotation]
case class FingerTreeSingle[+Element, Annotation](value: Element)
	extends FingerTree[Element, Annotation]

case class FingerTreeDeep[+Element, Annotation](
	prefix: Digit[Element],
	nextLayer: FingerTree[Branch[Element, Annotation], Annotation],
	suffix: Digit[Element],
	annotation: Annotation
) extends FingerTree[Element, Annotation] {
	def popLeftFromPrefix: Option[(Element, FingerTree[Element, Annotation])] =
		prefix.popLeft.map { case (head, tail) => (head, copy(prefix = tail)) }

	def popLeftFromNextLayer(implicit
		elementMeasurer: Measurer[Element, Annotation]
	): Option[(Element, FingerTree[Element, Annotation])] = {
		implicit val branchMeasurer: Measurer[Branch[Element, Annotation], Annotation] =
			elementMeasurer.toBranchMeasurer

		nextLayer.popLeft.map { case (head, tail) =>
			(prefix.value1, FingerTreeDeep(head.toDigit, tail, suffix))
		}
	}

	def popLeftFromSuffix(implicit
		measurer: Measurer[Element, Annotation]
	): (Element, FingerTree[Element, Annotation]) = (prefix.value1, suffix.toList)

	def popRightFromPrefix(implicit
		measurer: Measurer[Element, Annotation]
	): (FingerTree[Element, Annotation], Element) = (prefix.toList, suffix.value1)

	def popRightFromNextLayer(implicit
		elementMeasurer: Measurer[Element, Annotation]
	): Option[(FingerTree[Element, Annotation], Element)] = {
		implicit val branchMeasurer: Measurer[Branch[Element, Annotation], Annotation] =
			elementMeasurer.toBranchMeasurer

		nextLayer.popRight.map { case (init, last) =>
			(FingerTreeDeep(prefix, init, last.toDigit), suffix.value1)
		}
	}

	def popRightFromSuffix: Option[(FingerTree[Element, Annotation], Element)] =
		suffix.popRight.map { case (init, last) => (copy(suffix = init), last) }

	def splitByInNextLayer(predicate: Annotation => Boolean)(initialAnnotation: Annotation)(implicit
		elementMeasurer: Measurer[Element, Annotation]
	): Option[SplitPoint[Element, Annotation]] = {
		implicit val branchMeasurer: Measurer[Branch[Element, Annotation], Annotation] =
			elementMeasurer.toBranchMeasurer

		nextLayer.splitByWithInitial(predicate)(initialAnnotation).map {
			case SplitPoint(left, center, right) =>
				import elementMeasurer.annotationMonoid

				val leftMeasurer = branchMeasurer.toListMeasurer
				val centerSplitPoint = center
					.toList
					.splitByWithInitial(predicate)(initialAnnotation + leftMeasurer.measure(left))
					.getOrElse(throw new Exception("Expected a split point in center."))

				SplitPoint(
					FingerTreeDeep(Some(prefix), left, centerSplitPoint.left.toDigit),
					centerSplitPoint.center,
					FingerTreeDeep(centerSplitPoint.right.toDigit, right, Some(suffix))
				)
		}
	}

	def splitByInPrefix(predicate: Annotation => Boolean)(initialAnnotation: Annotation)(implicit
		elementMeasurer: Measurer[Element, Annotation]
	): Option[SplitPoint[Element, Annotation]] =
		prefix.toList.splitByWithInitial(predicate)(initialAnnotation).map { splitPoint =>
			splitPoint
				.copy(right = FingerTreeDeep(splitPoint.right.toDigit, nextLayer, Some(suffix)))
		}

	def splitByInSuffix(predicate: Annotation => Boolean)(initialAnnotation: Annotation)(implicit
		elementMeasurer: Measurer[Element, Annotation]
	): Option[SplitPoint[Element, Annotation]] =
		suffix.toList.splitByWithInitial(predicate)(initialAnnotation).map { splitPoint =>
			splitPoint.copy(left = FingerTreeDeep(Some(prefix), nextLayer, splitPoint.left.toDigit))
		}
}

object FingerTreeDeep {
	def apply[Element, Annotation](
		prefix: Digit[Element],
		nextLayer: FingerTree[Branch[Element, Annotation], Annotation],
		suffix: Digit[Element],
	)(implicit
		elementMeasurer: Measurer[Element, Annotation]
	): FingerTreeDeep[Element, Annotation] = {
		import elementMeasurer.annotationMonoid

		val branchMeasurer = elementMeasurer.toBranchMeasurer
		val digitMeasurer = elementMeasurer.toDigitMeasurer
		val nextLayerMeasurer = branchMeasurer.toListMeasurer

		FingerTreeDeep(
			prefix,
			nextLayer,
			suffix,
			digitMeasurer.measure(prefix) +
			nextLayerMeasurer.measure(nextLayer) +
			digitMeasurer.measure(suffix)
		)
	}

	def apply[Element, Annotation](
		prefix: Option[Digit[Element]],
		nextLayer: FingerTree[Branch[Element, Annotation], Annotation],
		suffix: Option[Digit[Element]],
	)(implicit
		elementMeasurer: Measurer[Element, Annotation]
	): FingerTree[Element, Annotation] = {
		implicit val branchMeasurer: Measurer[Branch[Element, Annotation], Annotation] =
			elementMeasurer.toBranchMeasurer

		(prefix, suffix) match {
			case (None, None) =>
				nextLayer
					.popRight
					.map { case (init, last) => apply(None, init, Some(last.toDigit))}
					.getOrElse(FingerTreeEmpty())

			case (None, Some(suffix)) =>
				nextLayer
					.popLeft
					.map { case (head, tail) => apply(head.toDigit, tail, suffix) }
					.getOrElse(suffix.toList)

			case (Some(prefix), None) =>
				nextLayer
					.popRight
					.map { case (init, last) => apply(prefix, init, last.toDigit) }
					.getOrElse(prefix.toList)

			case (Some(prefix), Some(suffix)) => apply(prefix, nextLayer, suffix)
		}
	}
}

/**
  * [[FingerTreeList]] is a wrapper around [[FingerTree]] enabling one to use it as a
  * sequential data structure.
  */
class FingerTreeList[+A](private[fingertrees] val underlying: FingerTree[A, Int]) {
	import FingerTreeList.elementMeasurer

	override def equals(other: Any): Boolean = other match {
		case list: FingerTreeList[?] => popLeft == list.popLeft
		case _ => false
	}

	override def toString: String = {
		def fingerTreeListToList(list: FingerTreeList[A]): List[A] = list
			.popLeft
			.map { case (head, tail) => head +: fingerTreeListToList(tail) }
			.getOrElse(List.empty)

		fingerTreeListToList(this).mkString("FingerTreeList(", ", ", ")")
	}

	def append[B >: A](other: B): FingerTreeList[B] = new FingerTreeList(underlying.append(other))
	def concatenate[B >: A](other: FingerTreeList[B]): FingerTreeList[B] =
		new FingerTreeList(underlying.concatenate(other.underlying))

	def get(i: Int): Option[A] =
		if (i < 0) None else underlying.splitByWithInitial(_ > i)(0).map(_.center)

	def head: Option[A] = underlying.head
	def init: Option[FingerTreeList[A]] = underlying.init.map(new FingerTreeList(_))
	def last: Option[A] = underlying.last
	def length = elementMeasurer.toListMeasurer.measure(underlying)
	def popLeft: Option[(A, FingerTreeList[A])] =
		underlying.popLeft.map { case (head, tail) => (head, new FingerTreeList(tail)) }

	def popRight: Option[(FingerTreeList[A], A)] =
		underlying.popRight.map { case (init, last) => (new FingerTreeList(init), last) }

	def prepend[B >: A](other: B): FingerTreeList[B] = new FingerTreeList(underlying.prepend(other))
	def tail: Option[FingerTreeList[A]] = underlying.tail.map(new FingerTreeList(_))
}

object FingerTreeList {
	private implicit def elementMeasurer[A]: Measurer[A, Int] = new Measurer[A, Int] {
		override def measure(element: A): Int = 1
	}

	lazy val empty: FingerTreeList[Nothing] = apply()

	def apply[A](elements: A*): FingerTreeList[A] = new FingerTreeList(
		elements match {
			case Seq() => FingerTreeEmpty()
			case Seq(value1) => FingerTreeSingle(value1)
			case Seq(value1, value2) =>
				FingerTreeDeep(Digit1(value1), FingerTreeEmpty[Int](), Digit1(value2))

			case Seq(value1, value2, value3) =>
				FingerTreeDeep(Digit1(value1), FingerTreeEmpty[Int](), Digit2(value2, value3))

			case Seq(value1, value2, value3, value4) =>
				FingerTreeDeep(
					Digit1(value1),
					FingerTreeEmpty[Int](),
					Digit3(value2, value3, value4)
				)

			case Seq(value1, value2, value3, value4, value5) =>
				FingerTreeDeep(
					Digit1(value1),
					FingerTreeEmpty[Int](),
					Digit4(value2, value3, value4, value5)
				)

			case Seq(value1, value2, value3, value4, value5, value6) =>
				FingerTreeDeep(
					Digit2(value1, value2),
					FingerTreeEmpty[Int](),
					Digit4(value3, value4, value5, value6)
				)

			case Seq(value1, value2, value3, value4, value5, value6, value7) =>
				FingerTreeDeep(
					Digit3(value1, value2, value3),
					FingerTreeEmpty[Int](),
					Digit4(value4, value5, value6, value7)
				)

			case _ =>
				FingerTreeDeep(
					Digit3(elements(0), elements(1), elements(2)),
					FingerTreeList(elements.slice(3, elements.length - 3): _*)
						.underlying
						.toBranchList,

					Digit3(
						elements(elements.length - 3),
						elements(elements.length - 2),
						elements(elements.length - 1)
					)
				)
		}
	)
}
