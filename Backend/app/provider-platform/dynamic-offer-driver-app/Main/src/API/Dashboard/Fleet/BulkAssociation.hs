{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Dashboard.Fleet.BulkAssociation (API, handler) where

import qualified Domain.Action.Dashboard.Fleet.BulkAssociation as Domain
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant

type API = Domain.BulkFleetAssociationAPI

handler :: ShortId DM.Merchant -> Context.City -> ServerT API FlowHandler
handler merchantId city = hoistServer (Proxy :: Proxy API) withFlowHandlerAPI (Domain.bulkFleetAssociationHandler merchantId city)
