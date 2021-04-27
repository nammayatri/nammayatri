module Utils.Common
  ( module Common,
    withFlowHandlerAPI,
    withFlowHandlerBecknAPI,
  )
where

import Beckn.Types.App (EnvR (..), FlowHandlerR)
import Beckn.Utils.Common as Common hiding (withFlowHandlerAPI, withFlowHandlerBecknAPI)
import EulerHS.Prelude

withFlowHandler :: FlowR r a -> FlowHandlerR r a
withFlowHandler flow = do
  (EnvR flowRt appEnv) <- ask
  liftIO . runFlowR flowRt appEnv $ flow

withFlowHandlerAPI :: FlowR r a -> FlowHandlerR r a
withFlowHandlerAPI = withFlowHandler . Common.apiHandler

withFlowHandlerBecknAPI :: FlowR r a -> FlowHandlerR r a
withFlowHandlerBecknAPI = withFlowHandler . Common.becknApiHandler
