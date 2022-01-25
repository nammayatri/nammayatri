module Beckn.Utils.MVar where

import Beckn.Prelude
import qualified Control.Concurrent.MVar as IO

type MVar = IO.MVar

newEmptyMVar :: MonadIO m => m (MVar a)
newEmptyMVar = liftIO IO.newEmptyMVar

newMVar :: MonadIO m => a -> m (MVar a)
newMVar = liftIO . IO.newMVar

takeMVar :: MonadIO m => MVar a -> m a
takeMVar = liftIO . IO.takeMVar

readMVar :: MonadIO m => MVar a -> m a
readMVar = liftIO . IO.readMVar

putMVar :: MonadIO m => MVar a -> a -> m ()
putMVar m = liftIO . IO.putMVar m

tryTakeMVar :: MonadIO m => MVar a -> m (Maybe a)
tryTakeMVar = liftIO . IO.tryTakeMVar

tryPutMVar :: MonadIO m => MVar a -> a -> m Bool
tryPutMVar m = liftIO . IO.tryPutMVar m

tryReadMVar :: MonadIO m => MVar a -> m (Maybe a)
tryReadMVar = liftIO . IO.tryReadMVar

isEmptyMVar :: MonadIO m => MVar a -> m Bool
isEmptyMVar = liftIO . IO.isEmptyMVar

swapMVar :: MonadIO m => MVar a -> a -> m a
swapMVar m = liftIO . IO.swapMVar m

-- TODO: implement MonadMask for FlowR so we can write proper `with` and `modify` functions

modifyMVar' :: MonadIO m => MVar a -> (a -> (a, b)) -> m b
modifyMVar' m f = liftIO . IO.modifyMVar m $ pure . f

modifyMVar_' :: MonadIO m => MVar a -> (a -> a) -> m ()
modifyMVar_' m f = liftIO . IO.modifyMVar_ m $ pure . f
