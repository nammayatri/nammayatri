{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.RiderPlatform.Management.Endpoints.Media where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Data.Text
import qualified EulerHS.Types
import qualified Data.Singletons.TH
import qualified Data.Aeson



newtype GetMediaFileResponse
  = GetMediaFileResponse {content :: Data.Text.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
type API = GetMediaFile
type GetMediaFile = ("file" :> MandatoryQueryParam "filePath" Data.Text.Text :> Get ('[JSON]) GetMediaFileResponse)
newtype MediaAPIs = MediaAPIs {getMediaFile :: (Data.Text.Text -> EulerHS.Types.EulerClient GetMediaFileResponse)}
mkMediaAPIs :: (Client EulerHS.Types.EulerClient API -> MediaAPIs)
mkMediaAPIs mediaClient = (MediaAPIs {..})
                where getMediaFile = mediaClient
data MediaUserActionType
    = GET_MEDIA_FILE
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass ToSchema
instance ToJSON MediaUserActionType
    where toJSON (GET_MEDIA_FILE) = Data.Aeson.String "GET_MEDIA_FILE"
instance FromJSON MediaUserActionType
    where parseJSON (Data.Aeson.String "GET_MEDIA_FILE") = pure GET_MEDIA_FILE
          parseJSON _ = fail "GET_MEDIA_FILE expected"

$(Data.Singletons.TH.genSingletons [(''MediaUserActionType)])

