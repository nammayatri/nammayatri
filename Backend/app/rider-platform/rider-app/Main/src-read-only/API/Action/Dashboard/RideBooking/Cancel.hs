{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.Cancel
  ( API.Types.Dashboard.RideBooking.Cancel.API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Cancel
import qualified Domain.Action.Dashboard.RideBooking.Cancel
import qualified "this" Domain.Action.UI.Cancel
import qualified "this" Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.Cancel.API)
handler merchantId city = postCancelBooking merchantId city

postCancelBooking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Cancel.CancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCancelBooking a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Cancel.postCancelBooking a5 a4 a3 a2 a1
