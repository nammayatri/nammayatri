module Beckn.Utils.Shutdown where

import Beckn.Prelude
import Control.Concurrent.STM.TMVar
import GHC.Conc
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

type Shutdown = TMVar ()

handleShutdown :: Shutdown -> IO () -> IO ()
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
        putStrLn ("Shutting down by " <> reason :: Text)

mkShutdown :: IO Shutdown
mkShutdown = newEmptyTMVarIO
