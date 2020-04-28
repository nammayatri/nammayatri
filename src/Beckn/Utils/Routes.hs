module Beckn.Utils.Routes where

import EulerHS.Prelude
import Beckn.Types.App
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L

withFlowHandler :: L.Flow a -> FlowHandler a
withFlowHandler flow = do
  (Env flowRt) <- ask
  lift $ ExceptT $ try $ I.runFlow flowRt $ flow
