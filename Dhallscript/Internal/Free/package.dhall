let Free = ./Type

let Applicative =
      https://raw.githubusercontent.com/FormationAI/dhall-bhat/master/Applicative/Type

let Functor =
      https://raw.githubusercontent.com/FormationAI/dhall-bhat/master/Functor/Type

in    λ(f : Type → Type)
    → λ(a : Type)
    →   { iter =
            ./iter f a
        , iterA =
            λ(g : Type → Type) → λ(AG : Applicative g) → ./iterA f g AG a
        , retract =
            ./retract f a
        , wrap =
            ./wrap f a
        , liftF = \(FF : Functor f) -> ./liftF f FF a
        }
      ∧ ./monad f ⫽ ./transformer
