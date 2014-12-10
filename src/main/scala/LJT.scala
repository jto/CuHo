package cuho

import grammar._, syntax._

// http://www.loria.fr/~galmiche/=papers/Wollic98.ps.gz
object LJT {
  case class Node(f: Formulae, children: Set[Node] = Set())

  // definition 2
  def sub(f: Formulae): Node = f match {
    case n @ Nil ⊢ r => sub(r)
    case x @ Axiom(_) => Node(x)
    case n @ (a ∧ b) → q => Node(n, Set(sub(a ∧ b), sub(q), sub((a → b) → q)))
    case n @ (a ∨ b) → q => Node(n, Set(sub(a ∨ b), sub(q), sub(a → q), sub(b → q)))
    case n @ (a → b) → q => Node(n, Set(sub(a → b), sub(q), sub(b → q)))
    case n @ p ∧ q => Node(n, Set(sub(p), sub(q)))
    case n @ p ∨ q => Node(n, Set(sub(p), sub(q)))
    case n @ x → q => Node(n, Set(sub(x), sub(q)))
    case n => Node(n, Set())
  }

  def toSet(n: Node): Set[Formulae] =
    Set(n.f) ++ n.children.flatMap(toSet)

  def γ(n: Node) = {
    val ss = toSet(n)
    (f: Formulae) => ss.contains(f)
  }

  def search(n: Formulae): Seq[Formulae] = n match {
    case Axiom(_) => Seq.empty
    // case (a ∧ b) ⊢ g        => Seq(a, b) ⊢ g            // ∧-G
    // case ⊢ (a ∧ b)          => Seq(⊢(a), ⊢(b))          // ∧-R
    // case (a ∨ b) ⊢ g        => Seq(a ⊢ g, b ⊢ g)        // ∨-L
    // case Seq(x, x → b) ⊢ g  => Seq(Seq(x, b) ⊢ g)       // →-L1
    // case ((a ∧ b) → c) ⊢ g  => Seq(((a → b) → c) ⊢ g)   // →-L2
    // case ((a ∨ b) → c) → g  => Seq(Seq(a → b, b → c) ⊢ g) // →-L3
    case Seq((a → b) → c) ⊢ g  => Seq((b → c) ⊢ (a → b), c ⊢ g) // →-L3
    // case Seq(f) ⊢ g => Seq(Seq(f) ⊢ g) // F
    case Seq() ⊢ (a → b) => Seq(a ⊢ b) // →-R
    // case f @ (x1 ⊢ x2) if x1 == x2 => Seq(f) // Id
    // TODO: ∨-R1, ∨-R2, →-L4
    case _ => Seq.empty
  }

}