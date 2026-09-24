{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.NotifyRideInfo where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.Ride
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data NotificationType
  = WHATSAPP
  | SMS
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NotifyRideInfoRequest = NotifyRideInfoRequest {notificationType :: NotificationType, rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets NotifyRideInfoRequest where
  hideSecrets = Kernel.Prelude.identity

type API = ("notifyRideInfo" :> PostNotifyRideInfoNotifyRideInfo)

type PostNotifyRideInfoNotifyRideInfo =
  ( "notifyRideInfo" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> ReqBody '[JSON] NotifyRideInfoRequest
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

newtype NotifyRideInfoAPIs = NotifyRideInfoAPIs {postNotifyRideInfoNotifyRideInfo :: Kernel.Types.Id.Id Domain.Types.Person.Person -> NotifyRideInfoRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess}

mkNotifyRideInfoAPIs :: (Client EulerHS.Types.EulerClient API -> NotifyRideInfoAPIs)
mkNotifyRideInfoAPIs notifyRideInfoClient = (NotifyRideInfoAPIs {..})
  where
    postNotifyRideInfoNotifyRideInfo = notifyRideInfoClient

data NotifyRideInfoUserActionType
  = POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON NotifyRideInfoUserActionType where
  toJSON POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO = Data.Aeson.String "POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO"

instance FromJSON NotifyRideInfoUserActionType where
  parseJSON (Data.Aeson.String "POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO") = pure POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO
  parseJSON _ = fail "POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO expected"

$(Data.Singletons.TH.genSingletons [''NotifyRideInfoUserActionType])
