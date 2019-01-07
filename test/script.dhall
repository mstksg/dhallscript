let IOStream = ../Dhallscript/IOStream/Type

let map = ../Dhallscript/IOStream/functor

let IO = ../dhall-bhat/Free/Type IOStream

let io = ../dhall-bhat/Free/package.dhall IOStream

let iostream = ../Dhallscript/IOStream/package-lifted.dhall IO (io.liftF map)

in  io.bind Text {} iostream.getLine (λ(t : Text) →
      io.bind Text {} iostream.getLine (λ(u : Text) →
        iostream.putStrLn (t ++ u)
      )
    )
