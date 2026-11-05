{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SharedCabInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SharedCabInternal
import qualified Domain.Action.UI.SharedCabInternal
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.SharedCabSession
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.SharedCab.SessionView
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( "sharedCab" :> "routes" :> "demand" :> Header "token" Kernel.Prelude.Text :> ReqBody '[JSON] API.Types.UI.SharedCabInternal.SharedCabDemandReq
      :> Post
           '[JSON]
           SharedLogic.SharedCab.SessionView.SharedCabDemandResp
      :<|> "sharedCab"
      :> "cab"
      :> "sync"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           Domain.Types.SharedCabSession.CabUpdate
      :> Post
           '[JSON]
           Domain.Types.SharedCabSession.CabSyncResp
      :<|> "sharedCab"
      :> "session"
      :> "operations"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabSessionOperationReq
      :> Post
           '[JSON]
           Domain.Types.SharedCabSession.SharedCabSeatsResp
      :<|> "sharedCab"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "operations"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabBookingOperationReq
      :> Post
           '[JSON]
           SharedLogic.SharedCab.SessionView.SharedCabSession
      :<|> "sharedCab"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "callNumber"
      :> MandatoryQueryParam
           "driverId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleNumber"
           Kernel.Prelude.Text
      :> Header
           "token"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.UI.SharedCabInternal.RiderCallNumberResp
      :<|> "sharedCab"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "message"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabMessageReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postSharedCabRoutesDemand :<|> postSharedCabCabSync :<|> postSharedCabSessionOperations :<|> postSharedCabBookingOperations :<|> getSharedCabBookingCallNumber :<|> postSharedCabBookingMessage

postSharedCabRoutesDemand :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SharedCabInternal.SharedCabDemandReq -> Environment.FlowHandler SharedLogic.SharedCab.SessionView.SharedCabDemandResp)
postSharedCabRoutesDemand a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.postSharedCabRoutesDemand a2 a1

postSharedCabCabSync :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.SharedCabSession.CabUpdate -> Environment.FlowHandler Domain.Types.SharedCabSession.CabSyncResp)
postSharedCabCabSync a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.postSharedCabCabSync a2 a1

postSharedCabSessionOperations :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SharedCabInternal.SharedCabSessionOperationReq -> Environment.FlowHandler Domain.Types.SharedCabSession.SharedCabSeatsResp)
postSharedCabSessionOperations a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.postSharedCabSessionOperations a2 a1

postSharedCabBookingOperations :: (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SharedCabInternal.SharedCabBookingOperationReq -> Environment.FlowHandler SharedLogic.SharedCab.SessionView.SharedCabSession)
postSharedCabBookingOperations a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.postSharedCabBookingOperations a3 a2 a1

getSharedCabBookingCallNumber :: (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.SharedCabInternal.RiderCallNumberResp)
getSharedCabBookingCallNumber a4 a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.getSharedCabBookingCallNumber a4 a3 a2 a1

postSharedCabBookingMessage :: (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SharedCabInternal.SharedCabMessageReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSharedCabBookingMessage a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.postSharedCabBookingMessage a3 a2 a1
