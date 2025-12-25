{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.Maps
  ( API.Types.Dashboard.RideBooking.Maps.API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Maps
import qualified Domain.Action.Dashboard.RideBooking.Maps
import qualified "this" Domain.Action.UI.Maps
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.Maps.API)
handler merchantId city = postMapsAutoComplete merchantId city :<|> postMapsGetPlaceName merchantId city

postMapsAutoComplete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Maps.AutoCompleteReq -> Environment.FlowHandler Domain.Action.UI.Maps.AutoCompleteResp)
postMapsAutoComplete a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Maps.postMapsAutoComplete a4 a3 a2 a1

postMapsGetPlaceName :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Maps.GetPlaceNameReq -> Environment.FlowHandler Domain.Action.UI.Maps.GetPlaceNameResp)
postMapsGetPlaceName a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Maps.postMapsGetPlaceName a4 a3 a2 a1
