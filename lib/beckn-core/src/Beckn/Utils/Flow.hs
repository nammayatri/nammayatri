module Beckn.Utils.Flow where

import Beckn.Types.Flow
import Beckn.Utils.Logging
import Control.Monad.Reader
import qualified EulerHS.Language as L
import EulerHS.Prelude

fork :: Text -> FlowR r () -> FlowR r ()
fork desc f = do
  env <- ask
  lift $ L.forkFlow desc $ handleExc env $ runReaderT f env
  where
    handleExc env =
      L.runSafeFlow >=> (`whenLeft` (\e -> runReaderT (err e) env))
    err e =
      logError $ "Thread " <> show desc <> " died with error: " <> show e

runSafeFlow :: FlowR r a -> FlowR r (Either Text a)
runSafeFlow flow = do
  env <- ask
  lift $ L.runSafeFlow $ runReaderT flow env
