{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Ride
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.Ride
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("ride" :> GetRideList)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRideList merchantId city

type GetRideList = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('RIDES) ('RIDE_LIST) :> API.Types.ProviderPlatform.Management.Ride.GetRideList)

getRideList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Ride.BookingStatus) -> Kernel.Prelude.Maybe (Kernel.Types.Common.Currency) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe ((Kernel.Types.Id.ShortId Dashboard.Common.Ride)) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideListRes)
getRideList merchantShortId opCity apiTokenInfo a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Ride.getRideList merchantShortId opCity apiTokenInfo a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
