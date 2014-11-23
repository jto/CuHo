package cuho

import grammar._, syntax._

// http://www.loria.fr/~galmiche/=papers/Wollic98.ps.gz
object LJT {
  case class Node(f: Formulae, children: Set[Node] = Set())

  // definition 2
  def sub(f: Formulae): Node = f match {
    case n @ Nil ⊢ r => sub(r)
    case x @Atom(_) => Node(x)
    case n @ (a ∧ b) → q => Node(n, Set(sub(a ∧ b), sub(q), sub((a → b) → q)))
    case n @ (a ∨ b) → q => Node(n, Set(sub(a ∨ b), sub(q), sub(a → q), sub(b → q)))
    case n @ (a → b) → q => Node(n, Set(sub(a → b), sub(q), sub(b → q)))
    case n @ p ∧ q => Node(n, Set(sub(p), sub(q)))
    case n @ p ∨ q => Node(n, Set(sub(p), sub(q)))
    case n @ x → q => Node(n, Set(sub(x), sub(q)))
  }
}