let Applicative =
      https://raw.githubusercontent.com/FormationAI/dhall-bhat/master/Applicative/Type
let Free = ./Type
in  \(f : Type -> Type)
    -> \(a : Type)
    -> { iter  = ./iter f a
       , iterA = \(g : Type -> Type) -> \(AG : Applicative g) -> ./iterA f g a AG
       , retract = ./retract f a
       } /\ ./monad f
