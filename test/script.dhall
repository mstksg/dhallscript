let dsio = ../Dhallscript/DSIO/package.dhall

in  dsio.bind Text {} dsio.getLine (\(t : Text) ->
        dsio.bind Text {} dsio.getLine (\(u : Text) ->
          dsio.putStrLn (t ++ u)
        )
    )
