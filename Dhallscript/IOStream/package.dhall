let IOStream = ./Type

let lifter = λ(a : Type) → λ(x : IOStream a) → x

in  ./package-lifted.dhall IOStream lifter ∧ ./functor
