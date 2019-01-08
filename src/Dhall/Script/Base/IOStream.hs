{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
import           Data.Functor.Contravariant
import           Data.Kind
import           Data.Map                   (Map)
import           Data.Text                  (Text)
import           Dhall as D hiding          (maybe)
import           Dhall.Core                 as DC
import           Dhall.TypeCheck            (X)
import           GHC.Generics               (Generic)
import           System.IO
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Dhall.Map                  as MD

newtype DSHandle = DSHandle { dshId :: Natural }
  deriving (Show, Eq, Ord, Generic)

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
  deriving (Functor, Generic)

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

instance Interpret DSHandle where
    autoWith _ = dsHandleType

dsHandleType :: D.Type DSHandle
dsHandleType = DSHandle <$> natural

instance Inject DSHandle where
    injectWith o = dshId >$< injectWith o

ioModeType :: D.Type IOMode
ioModeType = union . mconcat $
    [ ReadMode      <$ constructor "ReadMode"      unit
    , WriteMode     <$ constructor "WriteMode"     unit
    , AppendMode    <$ constructor "AppendMode"    unit
    , ReadWriteMode <$ constructor "ReadWriteMode" unit
    ]

ioStreamType :: Interpret a => InterpretOptions -> D.Type (IOStream a)
ioStreamType o = union $ mconcat
    [ constructor "HGetLine" . record $
        HGetLine <$> field "handle"   dsHandleType
                 <*> field "continue" (autoWith o)
    , constructor "HPutStr"  . record $
        HPutStr  <$> field "handle"   dsHandleType
                 <*> field "text"     strictText
                 <*> field "continue" (autoWith o)
    , constructor "OpenFile" . record $
        OpenFile <$> field "path"     strictText
                 <*> field "ioMode"   ioModeType
                 <*> field "continue" (autoWith o)
    , constructor "HClose"   . record $
        HClose   <$> field "handle"   dsHandleType
                 <*> field "continue" (autoWith o)
    ]

instance Interpret a => Interpret (IOStream a) where
    autoWith = ioStreamType

-- ioStreamInputType :: Inject a => InterpretOptions -> InputType (IOStream a)
-- ioStreamInputType o = inputUnion $ mconcat
--     [ inputConstructorWith
--         "HGetLine"
--         (inputRecord $ inputField "handle" >*< inputFieldWith "continue" _)
--         (\case HGetLine h c -> Just (h, c); _ -> Nothing)
--     ]
