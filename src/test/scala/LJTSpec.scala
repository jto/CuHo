package cuho

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import syntax._

class HelloSpec extends FlatSpec with ShouldMatchers {
  import LJT._
  val A = Axiom("A")
  val C = Axiom("C")
  "LJT" should "create subformuleas tree" in {

      val sn =
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

      val Γ = sub(⊢(((A → A) → C) → C))
      Γ should be === sn

      γ(Γ)((A → A)) should be === true
      γ(Γ)((C → A)) should be === false
    }

  it should "search proof" in {
    search(C) should be === Seq.empty
    search(⊢(((A → A) → C) → C)) should be === Seq(((A → A) → C) ⊢ C)
    search(((A → A) → C) ⊢ C) should be === Seq((A → C) ⊢ (A → A), C ⊢ C)
    // search((A → C) ⊢ (A → A)) should be === Seq(A, A → C) → A
  }
}
