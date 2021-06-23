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
      try >=> (`whenLeft` (\e -> runReaderT (err e) env))
    err (e :: SomeException) =
      logError $ "Thread " <> show desc <> " died with error: " <> show e