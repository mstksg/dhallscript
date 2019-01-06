{ getLine =
    ./getLine
, putStr =
    ./putStr
, putStrLn =
    λ(t : Text) → ./putStr (t ++ "\n")
, hGetLine =
    ./hGetLine
, hPutStr =
    ./hPutStr
, openFile =
    ./openFile
, hClose =
    ./hClose
, ffi =
    ./ffi
}
