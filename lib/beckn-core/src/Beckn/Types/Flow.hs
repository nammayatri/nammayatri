module Beckn.Types.Flow where

import EulerHS.Language as L
import EulerHS.Prelude

type FlowR r = ReaderT r L.Flow