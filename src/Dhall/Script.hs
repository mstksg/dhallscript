{-# LANGUAGE EmptyCase      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeInType     #-}
{-# LANGUAGE TypeOperators  #-}

module Dhall.Script (
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Operational
import           Data.Functor.Sum
import           Data.Kind
import           Data.Text                 (Text)
import           System.IO
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T

data DSHandle = DSHStdIn
              | DSHStdOut
              | DSHStdErr
              | DSHRaw Handle

-- todo: errors
data DSIOPrim :: Type -> Type where
    DSHGetLine  :: DSHandle -> DSIOPrim Text
    DSHPutStr   :: DSHandle -> Text -> DSIOPrim ()
    DSOpenFile  :: FilePath -> IOMode -> DSIOPrim DSHandle
    DSHClose    :: DSHandle -> DSIOPrim ()

type DSIO f = Program (DSIOPrim `Sum` f)

dsHandle :: DSHandle -> Handle
dsHandle DSHStdIn   = stdin
dsHandle DSHStdOut  = stdout
dsHandle DSHStdErr  = stderr
dsHandle (DSHRaw h) = h

dsioPrim
    :: DSIOPrim a
    -> IO a
dsioPrim (DSHGetLine h)   = T.hGetLine $ dsHandle h
dsioPrim (DSHPutStr h t)  = T.hPutStr (dsHandle h) t
dsioPrim (DSOpenFile p m) = DSHRaw <$> openFile p m
dsioPrim (DSHClose h)     = hClose $ dsHandle h

runDSIO
    :: MonadIO m
    => (forall x. f x -> m x)
    -> DSIO f a
    -> m a
runDSIO ffi = interpretWithMonad $ \case
    InL d -> liftIO $ dsioPrim d
    InR x -> ffi x


