module Beckn.Types.Time where

import Beckn.Types.Flow
import Data.Time (UTCTime)
import qualified Data.Time as Time
import EulerHS.Language as L
import EulerHS.Prelude

class Monad m => MonadTime m where
  getCurrentTime :: m UTCTime

instance MonadTime (FlowR r) where
  getCurrentTime = L.runIO Time.getCurrentTime

instance MonadTime IO where
  getCurrentTime = Time.getCurrentTime
