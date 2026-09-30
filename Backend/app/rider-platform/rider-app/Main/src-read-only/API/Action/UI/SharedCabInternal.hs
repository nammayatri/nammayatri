{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SharedCabInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SharedCabInternal
import qualified Data.Time
import qualified Domain.Action.UI.SharedCabInternal
import qualified Domain.Types.FRFSTicketBooking
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.SharedCab.SessionView
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "sharedCab" :> "routes" :> MandatoryQueryParam "agencyId" Kernel.Prelude.Text :> MandatoryQueryParam "lat" Kernel.Prelude.Double
      :> MandatoryQueryParam
           "lon"
           Kernel.Prelude.Double
      :> Header "token" Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabRoutesResp
      :<|> "sharedCab"
      :> "route"
      :> "select"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.SharedCabInternal.SelectRouteReq
      :> Post
           '[JSON]
           API.Types.UI.SharedCabInternal.SelectRouteResp
      :<|> "sharedCab"
      :> "session"
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
           SharedLogic.SharedCab.SessionView.SharedCabSession
      :<|> "sharedCab"
      :> "seats"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.SharedCabInternal.SeatsReq
      :> Post
           '[JSON]
           SharedLogic.SharedCab.SessionView.SharedCabSession
      :<|> "sharedCab"
      :> "route"
      :> "end"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           SharedLogic.SharedCab.SessionView.EndRouteReq
      :> Post
           '[JSON]
           (Kernel.Prelude.Maybe SharedLogic.SharedCab.SessionView.SharedCabSession)
      :<|> "sharedCab"
      :> "resume"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabDriverReq
      :> Post
           '[JSON]
           SharedLogic.SharedCab.SessionView.SharedCabSession
      :<|> "sharedCab"
      :> "trips"
      :> MandatoryQueryParam
           "date"
           Data.Time.Day
      :> MandatoryQueryParam
           "driverId"
           Kernel.Prelude.Text
      :> Header
           "token"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabTripsResp
      :<|> "sharedCab"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "cancel"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabDriverReq
      :> Post
           '[JSON]
           SharedLogic.SharedCab.SessionView.SharedCabSession
      :<|> "sharedCab"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "boardedWithoutCode"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabDriverReq
      :> Post
           '[JSON]
           SharedLogic.SharedCab.SessionView.SharedCabSession
      :<|> "sharedCab"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "dropped"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabDriverReq
      :> Post
           '[JSON]
           SharedLogic.SharedCab.SessionView.SharedCabSession
      :<|> "sharedCab"
      :> "cabFull"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabDriverReq
      :> Post
           '[JSON]
           SharedLogic.SharedCab.SessionView.SharedCabSession
  )

handler :: Environment.FlowServer API
handler = getSharedCabRoutes :<|> postSharedCabRouteSelect :<|> getSharedCabSession :<|> postSharedCabSeats :<|> postSharedCabRouteEnd :<|> postSharedCabResume :<|> getSharedCabTrips :<|> postSharedCabBookingCancel :<|> postSharedCabBookingBoardedWithoutCode :<|> postSharedCabBookingDropped :<|> postSharedCabCabFull

getSharedCabRoutes :: (Kernel.Prelude.Text -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.SharedCabInternal.SharedCabRoutesResp)
getSharedCabRoutes a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.getSharedCabRoutes a4 a3 a2 a1

postSharedCabRouteSelect :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SharedCabInternal.SelectRouteReq -> Environment.FlowHandler API.Types.UI.SharedCabInternal.SelectRouteResp)
postSharedCabRouteSelect a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.postSharedCabRouteSelect a2 a1

getSharedCabSession :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler SharedLogic.SharedCab.SessionView.SharedCabSession)
getSharedCabSession a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.getSharedCabSession a3 a2 a1

postSharedCabSeats :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SharedCabInternal.SeatsReq -> Environment.FlowHandler SharedLogic.SharedCab.SessionView.SharedCabSession)
postSharedCabSeats a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.postSharedCabSeats a2 a1

postSharedCabRouteEnd :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> SharedLogic.SharedCab.SessionView.EndRouteReq -> Environment.FlowHandler (Kernel.Prelude.Maybe SharedLogic.SharedCab.SessionView.SharedCabSession))
postSharedCabRouteEnd a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.postSharedCabRouteEnd a2 a1

postSharedCabResume :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SharedCabInternal.SharedCabDriverReq -> Environment.FlowHandler SharedLogic.SharedCab.SessionView.SharedCabSession)
postSharedCabResume a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.postSharedCabResume a2 a1

getSharedCabTrips :: (Data.Time.Day -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.SharedCabInternal.SharedCabTripsResp)
getSharedCabTrips a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.getSharedCabTrips a3 a2 a1

postSharedCabBookingCancel :: (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SharedCabInternal.SharedCabDriverReq -> Environment.FlowHandler SharedLogic.SharedCab.SessionView.SharedCabSession)
postSharedCabBookingCancel a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.postSharedCabBookingCancel a3 a2 a1

postSharedCabBookingBoardedWithoutCode :: (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SharedCabInternal.SharedCabDriverReq -> Environment.FlowHandler SharedLogic.SharedCab.SessionView.SharedCabSession)
postSharedCabBookingBoardedWithoutCode a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.postSharedCabBookingBoardedWithoutCode a3 a2 a1

postSharedCabBookingDropped :: (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SharedCabInternal.SharedCabDriverReq -> Environment.FlowHandler SharedLogic.SharedCab.SessionView.SharedCabSession)
postSharedCabBookingDropped a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.postSharedCabBookingDropped a3 a2 a1

postSharedCabCabFull :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SharedCabInternal.SharedCabDriverReq -> Environment.FlowHandler SharedLogic.SharedCab.SessionView.SharedCabSession)
postSharedCabCabFull a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.postSharedCabCabFull a2 a1
