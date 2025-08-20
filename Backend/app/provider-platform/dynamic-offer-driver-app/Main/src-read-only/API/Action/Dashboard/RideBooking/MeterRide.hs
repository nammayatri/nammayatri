{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.MeterRide
  ( API.Types.Dashboard.RideBooking.MeterRide.API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.MeterRide
import qualified "this" API.Types.UI.PriceBreakup
import qualified Domain.Action.Dashboard.RideBooking.MeterRide
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.MeterRide.API)
handler merchantId city = getMeterRidePrice merchantId city

getMeterRidePrice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.FlowHandler API.Types.UI.PriceBreakup.MeterRidePriceRes)
getMeterRidePrice a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.MeterRide.getMeterRidePrice a3 a2 a1
