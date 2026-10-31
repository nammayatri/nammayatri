module Domain.Action.UI.SharedCab
  ( getPublicTransportData,
    postSharedCabRoutesDemand,
    postSharedCabRouteOperations,
    getSharedCabSession,
    postSharedCabSessionOperations,
    postSharedCabBookingOperations,
    postSharedCabBookingCall,
    postSharedCabBookingMessage,
    getSharedCabCallStatus,
  )
where

import API.Types.UI.SharedCab
import qualified Domain.Action.UI.Call as DCall
import qualified Domain.Action.UI.SharedCabBooking as DBooking
import qualified Domain.Action.UI.SharedCabCall as DCallRider
import Domain.Action.UI.SharedCabExtra (TokenAuthInfo, requireDriver)
import qualified Domain.Action.UI.SharedCabExtra as Extra
import qualified Domain.Types.CallStatus as SCS
import Domain.Types.SharedCabSession (SharedCabDemandResp, SharedCabRouteDataResp, SharedCabSession)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id

getPublicTransportData :: TokenAuthInfo -> Flow SharedCabRouteDataResp
getPublicTransportData auth = requireDriver auth >>= Extra.getSharedCabRouteData

postSharedCabRoutesDemand :: TokenAuthInfo -> SharedCabDemandReq -> Flow SharedCabDemandResp
postSharedCabRoutesDemand auth req = requireDriver auth >>= (`Extra.postSharedCabRoutesDemand` req)

postSharedCabRouteOperations :: TokenAuthInfo -> SharedCabRouteOperationReq -> Flow SelectRouteResp
postSharedCabRouteOperations auth req = requireDriver auth >>= (`Extra.routeOperation` req)

getSharedCabSession :: TokenAuthInfo -> Flow (Maybe SharedCabSession)
getSharedCabSession auth = requireDriver auth >>= Extra.getSharedCabSession

postSharedCabSessionOperations :: TokenAuthInfo -> SharedCabSessionOperationReq -> Flow SharedCabSession
postSharedCabSessionOperations auth req = requireDriver auth >>= (`Extra.sessionOperation` req)

postSharedCabBookingOperations :: TokenAuthInfo -> Text -> SharedCabBookingOperationReq -> Flow SharedCabSession
postSharedCabBookingOperations auth bookingId req = requireDriver auth >>= \driver -> DBooking.bookingOperation driver bookingId req

postSharedCabBookingCall :: TokenAuthInfo -> Text -> Flow DCall.CallRes
postSharedCabBookingCall auth bookingId = requireDriver auth >>= (`DCallRider.callRider` bookingId)

postSharedCabBookingMessage :: TokenAuthInfo -> Text -> SharedCabMessageReq -> Flow APISuccess
postSharedCabBookingMessage auth bookingId req = requireDriver auth >>= \driver -> DCallRider.sendMessage driver bookingId req

getSharedCabCallStatus :: TokenAuthInfo -> Id SCS.CallStatus -> Flow DCall.GetCallStatusRes
getSharedCabCallStatus _ = DCall.getCallStatus
