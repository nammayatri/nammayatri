module Beckn.Utils.Cron where

import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Utils.Common
import Control.Monad.Reader
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)

authenticate ::
  HasFlowEnv m r '["cronAuthKey" ::: Maybe CronAuthKey] =>
  Maybe CronAuthKey ->
  m ()
authenticate rauth = do
  key <- asks (.cronAuthKey)
  let parsed = T.stripPrefix "Basic " =<< rauth
  unless (key == parsed) $ throwError (AuthBlocked "Bad auth key")
