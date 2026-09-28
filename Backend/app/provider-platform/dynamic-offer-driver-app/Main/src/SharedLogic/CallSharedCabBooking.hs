-- | driver-app -> rider-app client for the driver's actions on one booking (routes-browse's
-- POST /internal/sharedCab/booking/{bookingId}/{action}) and R19's cab-full (alloc-opus, log.md:1080:
-- POST /internal/sharedCab/cabFull, same mount/token as /sharedCab/resume). Header token, body
-- {driverId, vehicleNumber}, response SharedCabSession. Errors pass through as SharedCabBAPError: the
-- rider app's errorCode and HTTP status reach the driver app unchanged (4.5 R11).
module SharedLogic.CallSharedCabBooking
  ( BAPDriverReq (..),
    postBookingAction,
    postCabFull,
  )
where

import API.Types.UI.SharedCab (SharedCabSession)
import qualified Data.HashMap.Strict as HM
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import qualified Kernel.Utils.Servant.Client as EC
import Servant hiding (throwError)
import Tools.Error (SharedCabBAPError)

data BAPDriverReq = BAPDriverReq
  { driverId :: Text,
    vehicleNumber :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

type BookingActionAPI =
  "internal"
    :> "sharedCab"
    :> "booking"
    :> Capture "bookingId" Text
    :> Capture "action" Text
    :> Header "token" Text
    :> ReqBody '[JSON] BAPDriverReq
    :> Post '[JSON] SharedCabSession

type CabFullAPI =
  "internal"
    :> "sharedCab"
    :> "cabFull"
    :> Header "token" Text
    :> ReqBody '[JSON] BAPDriverReq
    :> Post '[JSON] SharedCabSession

type BAPFlow m r =
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  )

-- | `action` is the last path segment: cancel | boardedWithoutCode | dropped.
postBookingAction :: BAPFlow m r => Text -> BaseUrl -> Text -> Text -> BAPDriverReq -> m SharedCabSession
postBookingAction apiKey internalUrl action bookingId req = do
  logInfo $ "CallSharedCabBooking: " <> action <> " booking " <> bookingId <> " by driver " <> req.driverId
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError
    (identity @SharedCabBAPError)
    Nothing
    (Just "BAP_INTERNAL_API_ERROR")
    (Just internalEndPointHashMap)
    internalUrl
    (ET.client (Proxy @BookingActionAPI) bookingId action (Just apiKey) req)
    ("SharedCabBooking:" <> action)
    (Proxy @BookingActionAPI)

postCabFull :: BAPFlow m r => Text -> BaseUrl -> BAPDriverReq -> m SharedCabSession
postCabFull apiKey internalUrl req = do
  logInfo $ "CallSharedCabBooking: cab full on " <> req.vehicleNumber <> " by driver " <> req.driverId
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError
    (identity @SharedCabBAPError)
    Nothing
    (Just "BAP_INTERNAL_API_ERROR")
    (Just internalEndPointHashMap)
    internalUrl
    (ET.client (Proxy @CabFullAPI) (Just apiKey) req)
    "SharedCabCabFull"
    (Proxy @CabFullAPI)
