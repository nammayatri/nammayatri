{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Ride
  ( API.Types.RiderPlatform.Management.Ride.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Dashboard.RiderPlatform.Ride
import qualified Domain.Action.Dashboard.Ride as Domain.Action.Dashboard.Ride
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.Ride.API)
handler merchantId city = getRideList merchantId city :<|> getRideRideinfo merchantId city :<|> getRideInfo merchantId city :<|> getRideRideInfo merchantId city

getRideList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Dashboard.RiderPlatform.Ride.BookingStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.RideListRes)
getRideList a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRideList a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getRideRideinfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.RideInfoRes)
getRideRideinfo a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRideRideinfo a3 a2 a1

getRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler Dashboard.RiderPlatform.Ride.ShareRideInfoRes)
getRideInfo a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRideInfo a3 a2 a1

getRideRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.ShortId Dashboard.Common.Ride -> Environment.FlowHandler Dashboard.RiderPlatform.Ride.ShareRideInfoRes)
getRideRideInfo a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRideRideInfo a3 a2 a1
