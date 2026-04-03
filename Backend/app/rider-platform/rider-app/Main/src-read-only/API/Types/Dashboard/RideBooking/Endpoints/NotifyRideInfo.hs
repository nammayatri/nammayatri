{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.Dashboard.RideBooking.Endpoints.NotifyRideInfo where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified "this" Domain.Types.Ride
import qualified "this" Domain.Types.Person
import qualified Kernel.Types.APISuccess
import qualified Kernel.Prelude
import qualified EulerHS.Types
import qualified Data.Singletons.TH
import qualified Kernel.Types.HideSecrets
import qualified Data.Aeson



data NotificationType
    = WHATSAPP | SMS
    deriving stock (Show, Eq, Ord, Read, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data NotifyRideInfoRequest
    = NotifyRideInfoRequest {notificationType :: NotificationType, rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
instance Kernel.Types.HideSecrets.HideSecrets NotifyRideInfoRequest
    where hideSecrets = Kernel.Prelude.identity
type API = ("notifyRideInfo" :> PostNotifyRideInfoNotifyRideInfo)
type PostNotifyRideInfoNotifyRideInfo = ("notifyRideInfo" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> ReqBody ('[JSON]) NotifyRideInfoRequest :> Post ('[JSON])
                                                                                                                                                                                     Kernel.Types.APISuccess.APISuccess)
newtype NotifyRideInfoAPIs
  = NotifyRideInfoAPIs {postNotifyRideInfoNotifyRideInfo :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> NotifyRideInfoRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}
mkNotifyRideInfoAPIs :: (Client EulerHS.Types.EulerClient API -> NotifyRideInfoAPIs)
mkNotifyRideInfoAPIs notifyRideInfoClient = (NotifyRideInfoAPIs {..})
                         where postNotifyRideInfoNotifyRideInfo = notifyRideInfoClient
data NotifyRideInfoUserActionType
    = POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass ToSchema
instance ToJSON NotifyRideInfoUserActionType
    where toJSON (POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO) = Data.Aeson.String "POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO"
instance FromJSON NotifyRideInfoUserActionType
    where parseJSON (Data.Aeson.String "POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO") = pure POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO
          parseJSON _ = fail "POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO expected"

$(Data.Singletons.TH.genSingletons [(''NotifyRideInfoUserActionType)])

