let Free = ./Type

let Applicative =
      https://raw.githubusercontent.com/FormationAI/dhall-bhat/master/Applicative/Type

let Functor =
      https://raw.githubusercontent.com/FormationAI/dhall-bhat/master/Functor/Type

in    λ(f : Type → Type)
    →   { iter =
            ./iter f
        , iterA =
            λ(g : Type → Type) → λ(AG : Applicative g) → ./iterA f g AG
        , retract =
            ./retract f
        , wrap =
            ./wrap f
        , liftF = \(FF : Functor f) -> ./liftF f FF
        , foldFree = ./foldFree f
        }
      ∧ ./monad f ⫽ ./transformer
