let handle = ./Handle/package.dhall

let Handle = ./Handle/Type

let IOMode = ./IOMode

let IOStream = ./Type

let Lifter = λ(g : Type → Type) → ∀(a : Type) → IOStream a → g a

let Module =
        λ(g : Type → Type)
      → λ(f : Lifter g)
      → { getLine =
            f Text (./hGetLine handle.stdin)
        , putStr =
            λ(t : Text) → f {} (./hPutStr handle.stdout t)
        , putStrLn =
            λ(t : Text) → f {} (./hPutStr handle.stdout (t ++ "\n"))
        , hGetLine =
            λ(h : Handle) → f Text (./hGetLine h)
        , hPutStr =
            λ(h : Handle) → λ(t : Text) → f {} (./hPutStr h t)
        , openFile =
            λ(p : Text) → λ(m : IOMode) → f Handle (./openFile p m)
        , hClose =
            λ(h : Handle) → f {} (./hClose h)
        }

in  Module
