module Utils.Common
  ( module Utils.Common,
    module Common,
  )
where

import Beckn.Types.App (FlowHandlerR)
import Beckn.Utils.Common as Common hiding (withFlowHandlerBecknAPI, withFlowHandlerAPI)

withFlowHandlerBecknAPI :: FlowR r a -> FlowHandlerR r a
withFlowHandlerBecknAPI = Common.withUnblockableFlowHandlerBecknAPI

withFlowHandlerAPI :: FlowR r a -> FlowHandlerR r a
withFlowHandlerAPI = Common.withUnblockableFlowHandlerAPI