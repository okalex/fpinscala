package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

	def size[A](t: Tree[A]): Int = t match {
		case Leaf(_) => 1
		case Branch(l, r) => size(l) + size(r) + 1
	}

	def maximum(t: Tree[Int]): Int = t match {
		case Leaf(v) => v
		case Branch(l, r) => maximum(l).max(maximum(r))
	}

	def depth[A](t: Tree[A]): Int = t match {
		case Leaf(_) => 0
		case Branch(l, r) => depth(l).max(depth(r)) + 1
	}

	def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
		case Leaf(a) => Leaf(f(a))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}

	def fold[A]

}

object TreeModule {
	import Tree._

	def main(args: Array[String]): Unit = {
		println("Welcome to TreeModule")

		val t1 = Branch(
					Branch(
						Leaf(1),
						Branch(
							Leaf(2),
							Branch(
								Leaf(3),
								Leaf(4)
							)
						)
					),
					Leaf(5)
				)

		println(s"size(t1) = ${size(t1)}")
		println(s"maximum(t1) = ${maximum(t1)}")
		println(s"depth(t1) = ${depth(t1)}")
		println(s"map(t1)(_ + 1) = ${map(t1)(_ + 1)}")
	}
}