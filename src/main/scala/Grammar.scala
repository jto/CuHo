package cuho


object grammar {
  sealed trait Formulae
  object ⊤ extends Formulae
  object ⊥ extends Formulae

  case class Atom(name: String) extends Formulae {
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
  implicit def toT[L <: Formulae, R <: Formulae](l: Seq[L]) = new {
    def ⊢[R <: Formulae](r: R) = grammar.⊢(l, r)
  }
  def ⊢[R <: Formulae](r: R) = grammar.⊢(Seq.empty, r)
  def Atom(name: String) = grammar.Atom(name)
}