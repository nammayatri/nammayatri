module Utils.Routes where

import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Types.App

withFlowHandler :: L.Flow a -> FlowHandler a
withFlowHandler flow = do
  (Env flowRt) <- ask
  lift $ ExceptT $ try $ I.runFlow flowRt $ flow
