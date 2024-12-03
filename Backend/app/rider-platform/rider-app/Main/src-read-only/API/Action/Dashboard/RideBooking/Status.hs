{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.Status
  ( API.Types.Dashboard.RideBooking.Status.API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Status
import qualified Domain.Action.Dashboard.RideBooking.Status as Domain.Action.Dashboard.RideBooking.Status
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.Status.API)
handler merchantId city = getStatusGetFRFSTicketStatus merchantId city

getStatusGetFRFSTicketStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
getStatusGetFRFSTicketStatus a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Status.getStatusGetFRFSTicketStatus a3 a2 a1
