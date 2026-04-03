{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.ProviderPlatform.Management.Endpoints.SosMedia where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Data.Text
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Dashboard.Common
import qualified EulerHS.Types
import qualified Data.Singletons.TH
import qualified Data.Aeson



data GetSosMediaResponse
    = GetSosMediaResponse {content :: Data.Text.Text,
                           createdAt :: Kernel.Prelude.UTCTime,
                           updatedAt :: Kernel.Prelude.UTCTime,
                           rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride,
                           ticketId :: Kernel.Prelude.Maybe Data.Text.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
type API = GetSosMediaSosMedia
type GetSosMediaSosMedia = ("sos" :> "media" :> Capture "personId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> Get ('[JSON]) [GetSosMediaResponse])
newtype SosMediaAPIs = SosMediaAPIs {getSosMediaSosMedia :: (Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient [GetSosMediaResponse])}
mkSosMediaAPIs :: (Client EulerHS.Types.EulerClient API -> SosMediaAPIs)
mkSosMediaAPIs sosMediaClient = (SosMediaAPIs {..})
                   where getSosMediaSosMedia = sosMediaClient
data SosMediaUserActionType
    = GET_SOS_MEDIA_SOS_MEDIA
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass ToSchema
instance ToJSON SosMediaUserActionType
    where toJSON (GET_SOS_MEDIA_SOS_MEDIA) = Data.Aeson.String "GET_SOS_MEDIA_SOS_MEDIA"
instance FromJSON SosMediaUserActionType
    where parseJSON (Data.Aeson.String "GET_SOS_MEDIA_SOS_MEDIA") = pure GET_SOS_MEDIA_SOS_MEDIA
          parseJSON _ = fail "GET_SOS_MEDIA_SOS_MEDIA expected"

$(Data.Singletons.TH.genSingletons [(''SosMediaUserActionType)])

