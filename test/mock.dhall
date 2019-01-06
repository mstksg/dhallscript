let State =
      https://raw.githubusercontent.com/FormationAI/dhall-bhat/master/State/Type
      (List Text)

let state =
      https://raw.githubusercontent.com/FormationAI/dhall-bhat/master/State/package.dhall
      (List Text)

let MState =
      https://raw.githubusercontent.com/FormationAI/dhall-bhat/master/State/monad
      (List Text)

let DSIO = ../Dhallscript/DSIO/Type

let IOMode = ../Dhallscript/DSIO/IOMode

let Handle = ../Dhallscript/Handle/Type

let DSIOF = ../Dhallscript/DSIO/DSIOF/Type

let free = ../Dhallscript/Internal/Free/package.dhall DSIOF

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
    : ∀(a : Type) → DSIOF a → State a
    =   λ(a : Type)
      → λ(d0 : DSIOF a)
      → merge
        { DSHGetLine =
              λ(d : { handle : Handle, continue : Text → a })
            → log a "[getting line]" (d.continue "hello")
        , DSHPutStr =
              λ(d : { handle : Handle, text : Text, continue : a })
            → log a d.text d.continue
        , DSOpenFile =
              λ(d : { path : Text, ioMode : IOMode, continue : Handle → a })
            → log a ("[open file " ++ d.path ++ "]") (d.continue 3)
        , DSHClose =
              λ(d : { handle : Handle, continue : a })
            → log a "[closing handle]" d.continue
        , DSFFI =
              λ(d : { ffi : Text, continue : Text → a })
            → log a ("[ffi " ++ d.ffi ++ "]") (d.continue "ffi result")
        }
        d0

let mock
    : ∀(a : Type) → DSIO a → { val : a, state : List Text }
    =   λ(a : Type)
      → λ(d0 : DSIO a)
      → free.foldFree State MState a interp d0 ([] : List Text)

in  mock
