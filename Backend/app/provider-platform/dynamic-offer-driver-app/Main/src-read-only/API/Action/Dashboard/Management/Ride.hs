{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Ride
  ( API.Types.ProviderPlatform.Management.Ride.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.Ride
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Ride.API)
handler merchantId city = getRideList merchantId city :<|> getRideAgentList merchantId city :<|> getRideListV2 merchantId city :<|> postRideEndMultiple merchantId city :<|> postRideCancelMultiple merchantId city :<|> getRideInfo merchantId city :<|> postRideSync merchantId city :<|> postRideSyncMultiple merchantId city :<|> postRideRoute merchantId city :<|> getRideKaptureList merchantId city :<|> getRideFareBreakUp merchantId city :<|> postRideWaiverRideCancellationPenalty merchantId city

getRideList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.BookingStatus -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideListRes)
getRideList a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideList a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getRideAgentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.BookingStatus -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideListRes)
getRideAgentList a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideAgentList a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getRideListV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.RideStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideListResV2)
getRideListV2 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideListV2 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postRideEndMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Ride.MultipleRideEndReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.MultipleRideEndResp)
postRideEndMultiple a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.postRideEndMultiple a3 a2 a1

postRideCancelMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Ride.MultipleRideCancelReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.MultipleRideCancelResp)
postRideCancelMultiple a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.postRideCancelMultiple a3 a2 a1

getRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideInfoRes)
getRideInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideInfo a3 a2 a1

postRideSync :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideSyncRes)
postRideSync a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.postRideSync a3 a2 a1

postRideSyncMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Ride.MultipleRideSyncReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.MultipleRideSyncRes)
postRideSyncMultiple a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.postRideSyncMultiple a3 a2 a1

postRideRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideRouteRes)
postRideRoute a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.postRideRoute a3 a2 a1

getRideKaptureList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.TicketRideListRes)
getRideKaptureList a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideKaptureList a6 a5 a4 a3 a2 a1

getRideFareBreakUp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.FareBreakUpRes)
getRideFareBreakUp a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideFareBreakUp a3 a2 a1

postRideWaiverRideCancellationPenalty :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.ProviderPlatform.Management.Ride.WaiverRideCancellationPenaltyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideWaiverRideCancellationPenalty a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.postRideWaiverRideCancellationPenalty a4 a3 a2 a1
