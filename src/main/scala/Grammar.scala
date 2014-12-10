package cuho

object grammar {
  sealed trait Formulae
  object ⊤ extends Formulae
  object ⊥ extends Formulae

  case class Axiom(name: String) extends Formulae {
    override def toString = name
  }
  case class ∧[L <: Formulae, R <: Formulae](l: L, r: R) extends Formulae {
    override def toString = s"($l ∧ $r)"
  }
  case class ∨[L <: Formulae, R <: Formulae](l: L, r: R) extends Formulae {
    override def toString = s"($l ∨ $r)"
  }
  case class →[L <: Formulae, R <: Formulae](l: L, r: R) extends Formulae {
    override def toString = s"($l → $r)"
  }
  case class ¬[R <: Formulae](r: R) extends Formulae {
    override def toString = s"¬$r"
  }
  case class ⊢[L <: Formulae, R <: Formulae](l: Seq[L], r: R) extends Formulae {
    override def toString = s"$l ⊢ $r"
  }
}


object syntax {
  import grammar.Formulae
  implicit def toConj[L <: Formulae](l: L) = new {
    def ∧[R <: Formulae](r: R) = grammar.∧(l, r)
  }
  implicit def toDij[L <: Formulae](l: L) = new {
    def ∨[R <: Formulae](r: R) = grammar.∨(l, r)
  }
  implicit def toImpl[L <: Formulae](l: L) = new {
    def →[R <: Formulae](r: R) = grammar.→(l, r)
  }
  implicit def toT1[L <: Formulae](ls: Seq[L]) = new {
    def ⊢[R <: Formulae](r: R) = grammar.⊢(ls, r)
  }
  implicit def toT2[L <: Formulae](l: L) = new {
    def ⊢[R <: Formulae](r: R) = grammar.⊢(Seq(l), r)
  }
  def ⊢[R <: Formulae](r: R) = grammar.⊢(Seq.empty, r)
  def Axiom(name: String) = grammar.Axiom(name)
}