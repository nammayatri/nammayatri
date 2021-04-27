{-# LANGUAGE TypeApplications #-}

module Beckn.Utils.App
  ( handleLeft,
    handleShutdown,
  )
where

import Beckn.Utils.Common
import Control.Concurrent.STM.TMVar
import qualified EulerHS.Language as L
import EulerHS.Prelude
import System.Exit (ExitCode)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

handleLeft :: forall a b m. (Show a, Log m, L.MonadFlow m) => ExitCode -> Text -> Either a b -> m b
handleLeft exitCode msg = \case
  Left err -> do
    logError (msg <> show err)
    L.runIO $ exitWith exitCode
  Right res -> return res

handleShutdown :: TMVar () -> IO () -> IO ()
handleShutdown shutdown closeSocket = do
  void $ installHandler sigTERM (Catch $ shutdownAction "sigTERM" >> closeSocket) Nothing
  void $ installHandler sigINT (Catch $ shutdownAction "sigINT" >> closeSocket) Nothing
  where
    shutdownAction reason = do
      isLocked <- atomically $ do
        isEmptyTMVar shutdown >>= \case
          True -> do
            putTMVar shutdown ()
            return True
          False -> return False
      when isLocked $ do
        putStrLn @String $ "Shutting down by " <> reason
