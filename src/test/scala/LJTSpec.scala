package cuho

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import syntax._

class HelloSpec extends FlatSpec with ShouldMatchers {
  import LJT._
  "LJT" should "create subformuleas tree" in {
    val A = Atom("A")
    val C = Atom("C")

    val expected =
      Node((((A → A) → C) → C), Set(
        Node(((A → A) → C), Set(
          Node((A → A), Set(
            Node(A))),
          Node(C),
          Node((A → C), Set(
            Node(A),
            Node(C))))),
        Node(C),
        Node((C → C), Set(
          Node(C)))))

    val ss = sub(⊢(((A → A) → C) → C))
    ss should be === expected
  }
}
