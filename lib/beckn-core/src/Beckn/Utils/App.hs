{-# LANGUAGE TypeApplications #-}

module Beckn.Utils.App where

import Beckn.Utils.Logging
import Control.Concurrent
import Control.Concurrent.STM.TMVar
import qualified EulerHS.Language as L
import EulerHS.Prelude
import System.Exit (ExitCode)

handleLeft :: forall a b m. (Show a, Log m, L.MonadFlow m) => ExitCode -> Text -> Either a b -> m b
handleLeft exitCode msg = \case
  Left err -> do
    logError (msg <> show err)
    L.runIO $ exitWith exitCode
  Right res -> return res

handleShutdown :: TVar Int -> TMVar () -> ExitCode -> ThreadId -> IO ()
handleShutdown activeConnections shutdown code threadId = do
  isLocked <- atomically $ do
    isEmptyTMVar shutdown >>= \case
      True -> do
        putTMVar shutdown ()
        return True
      False -> return False
  when isLocked $ do
    putStrLn @String "Draining connections"
    waitForDrain 120000000
    throwTo threadId code
  where
    waitForDrain :: Int -> IO ()
    waitForDrain ms = do
      conns <- readTVarIO activeConnections
      unless (ms == 0 || conns == 0) $ do
        -- Wait 100ms and recurse
        let sleep = 100000
        threadDelay sleep
        waitForDrain $ ms - sleep
