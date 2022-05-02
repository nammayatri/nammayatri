module Beckn.Utils.Shutdown where

import Beckn.Prelude
import Control.Concurrent.STM.TMVar
import GHC.Conc
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

type Shutdown = TMVar ()

handleShutdown :: Shutdown -> IO () -> IO () -> IO ()
handleShutdown shutdown onShutdown closeSocket = do
  void $ installHandler sigTERM (Catch $ shutdownAction "sigTERM") Nothing
  void $ installHandler sigINT (Catch $ shutdownAction "sigINT") Nothing
  where
    shutdownAction reason = do
      isLocked <- atomically $ do
        isEmptyTMVar shutdown >>= \case
          True -> do
            putTMVar shutdown ()
            return True
          False -> return False
      when isLocked $ do
        putStrLn ("Shutting down by " <> reason :: Text)
      onShutdown
      closeSocket

waitForShutdown :: Shutdown -> IO ()
waitForShutdown = atomically . takeTMVar

mkShutdown :: IO Shutdown
mkShutdown = newEmptyTMVarIO

untilShutdown ::
  ( MonadIO m,
    MonadReader r m,
    HasField "isShuttingDown" r Shutdown
  ) =>
  m () ->
  m ()
untilShutdown =
  whileM isRunning

isRunning ::
  ( MonadIO m,
    MonadReader r m,
    HasField "isShuttingDown" r Shutdown
  ) =>
  m Bool
isRunning = liftIO . atomically . isEmptyTMVar =<< asks (.isShuttingDown)
