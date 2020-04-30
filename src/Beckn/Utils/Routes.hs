module Beckn.Utils.Routes where

import           Beckn.Types.App
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language     as L
import           EulerHS.Prelude

withFlowHandler :: L.Flow a -> FlowHandler a
withFlowHandler flow = do
  (Env flowRt) <- ask
  lift $ ExceptT $ try $ I.runFlow flowRt $ flow
