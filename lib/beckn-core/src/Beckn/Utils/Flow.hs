module Beckn.Utils.Flow where

import Beckn.Types.Flow
import Beckn.Utils.Logging
import Control.Monad.Reader
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)

-- | A replacement for 'L.forkFlow' which works in 'FlowR'.
-- It's main use case is to perform an action asynchronously without waiting for
-- result.
--
-- It has several differences comparing to 'L.forkFlow':
-- * Logs errors in case if the action failed;
-- * Expects action to return '()' - this is good, because the opposite means
--   you ignored something important, e.g. an exception returned explicitly;
-- * Do not log the fact of thread creation (was it any useful?)
--
-- NOTE: this function is temporary, use of bare forking is bad and should be
-- removed one day.

-- I know it is looking similar to forkAsync but I found it simpler to
-- be able to use (FlowR r) instead of (FlowR (EnvR r))
fork :: Text -> FlowR r () -> FlowR r ()
fork desc f = do
  env <- ask
  lift $ L.forkFlow desc $ handleExc env $ runReaderT f env
  where
    handleExc env a =
      L.runSafeFlow a >>= \case
        Right () -> pass
        Left e -> runReaderT (err e) env
    err e =
      logTagWarning "Thread" $
        "Thread " <> show desc <> " died with error: " <> show e

runSafeFlow :: (FromJSON a, ToJSON a) => FlowR r a -> FlowR r (Either Text a)
runSafeFlow flow = do
  env <- ask
  lift $ L.runSafeFlow $ runReaderT flow env
