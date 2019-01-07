let State = ../dhall-bhat/State/Type (List Text)

let state = ../dhall-bhat/State/package.dhall (List Text)

let MState = ../dhall-bhat/State/monad (List Text)

let IOStream = ../Dhallscript/IOStream/Type

let IOMode = ../Dhallscript/IOStream/IOMode

let Handle = ../Dhallscript/IOStream/Handle/Type

let IO = ../dhall-bhat/Free/Type IOStream

let free = ../dhall-bhat/Free/package.dhall IOStream

let log
    : ∀(a : Type) → Text → a → State a
    =   λ(a : Type)
      → λ(t : Text)
      → λ(r : a)
      → state.map
        {}
        a
        (λ(_ : {}) → r)
        (state.modify (λ(l : List Text) → l # [ t ]))

let interp
    : ∀(a : Type) → IOStream a → State a
    =   λ(a : Type)
      → λ(d0 : IOStream a)
      → merge
        { HGetLine =
              λ(d : { handle : Handle, continue : Text → a })
            → log a "[getting line]" (d.continue "hello")
        , HPutStr =
              λ(d : { handle : Handle, text : Text, continue : a })
            → log a d.text d.continue
        , OpenFile =
              λ(d : { path : Text, ioMode : IOMode, continue : Handle → a })
            → log a ("[open file " ++ d.path ++ "]") (d.continue 3)
        , HClose =
              λ(d : { handle : Handle, continue : a })
            → log a "[closing handle]" d.continue
        }
        d0

let mock
    : ∀(a : Type) → IO a → { val : a, state : List Text }
    =   λ(a : Type)
      → λ(d0 : IO a)
      → free.foldFree State MState a interp d0 ([] : List Text)

in  mock
