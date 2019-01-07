{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Dhall.Script.Base.IOStream (
    DSHandle
  , HandleMap
  , newHandleMap
  , IOStream(..)
  , IOStreamErr(..)
  , runIOStream
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Kind
import           Data.Map               (Map)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           System.IO
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Data.Text.IO           as T

newtype DSHandle = DSHandle { dshId :: Int }
  deriving (Show, Eq, Ord)

newtype HandleMap = HandleMap { handleMap :: Map DSHandle Handle }

data IOStreamErr = IOSENoHandle DSHandle
  deriving (Show, Generic)

instance Exception IOStreamErr

newHandleMap :: HandleMap
newHandleMap = HandleMap $ M.fromList
    [ (DSHandle 0, stdin )
    , (DSHandle 1, stdout)
    , (DSHandle 2, stderr)
    ]

data IOStream a
    = HGetLine DSHandle        (Text -> a)
    | HPutStr  DSHandle Text   a
    | OpenFile Text     IOMode (DSHandle -> a)
    | HClose   DSHandle        a
  deriving Functor

runIOStream
    :: (MonadIO m, MonadState HandleMap m, MonadThrow m)
    => IOStream a
    -> m a
runIOStream = \case
    HGetLine h c -> gets (M.lookup h . handleMap) >>= \case
      Just h' -> c <$> liftIO (T.hGetLine h')
      Nothing -> throwM (IOSENoHandle h)
    HPutStr h t c -> gets (M.lookup h . handleMap) >>= \case
      Just h' -> c <$  liftIO (T.hPutStr h' t)
      Nothing -> throwM (IOSENoHandle h)
    OpenFile fp m c -> do
      h   <- liftIO $ openFile (T.unpack fp) m
      dsh <- gets $ DSHandle . maybe 0 ((+ 1) . dshId . fst) . M.lookupMax . handleMap
      c dsh <$ modify (HandleMap . M.insert dsh h . handleMap)
    HClose h c -> gets (M.lookup h . handleMap) >>= \case
      Just h' -> c <$  liftIO (hClose h')
      Nothing -> throwM (IOSENoHandle h)
