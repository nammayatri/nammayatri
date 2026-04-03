{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.ProviderPlatform.Management.Endpoints.Media where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified "shared-services" IssueManagement.Domain.Types.MediaFile
import qualified EulerHS.Types
import qualified Data.Singletons.TH
import qualified Data.Aeson



data GetImageResponse
    = GetImageResponse {imageBase64 :: Kernel.Prelude.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
type API = GetMediaMediaImage
type GetMediaMediaImage = ("media" :> "image" :> Capture "imageId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile) :> Get ('[JSON]) GetImageResponse)
newtype MediaAPIs = MediaAPIs {getMediaMediaImage :: (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> EulerHS.Types.EulerClient GetImageResponse)}
mkMediaAPIs :: (Client EulerHS.Types.EulerClient API -> MediaAPIs)
mkMediaAPIs mediaClient = (MediaAPIs {..})
                where getMediaMediaImage = mediaClient
data MediaUserActionType
    = GET_MEDIA_MEDIA_IMAGE
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass ToSchema
instance ToJSON MediaUserActionType
    where toJSON (GET_MEDIA_MEDIA_IMAGE) = Data.Aeson.String "GET_MEDIA_MEDIA_IMAGE"
instance FromJSON MediaUserActionType
    where parseJSON (Data.Aeson.String "GET_MEDIA_MEDIA_IMAGE") = pure GET_MEDIA_MEDIA_IMAGE
          parseJSON _ = fail "GET_MEDIA_MEDIA_IMAGE expected"

$(Data.Singletons.TH.genSingletons [(''MediaUserActionType)])

