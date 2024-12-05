{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Media where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified "shared-services" IssueManagement.Domain.Types.MediaFile
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data GetImageResponse = GetImageResponse {imageBase64 :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = GetMediaMediaImage

type GetMediaMediaImage = ("media" :> "image" :> Capture "imageId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile) :> Get '[JSON] GetImageResponse)

newtype MediaAPIs = MediaAPIs {getMediaMediaImage :: Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> EulerHS.Types.EulerClient GetImageResponse}

mkMediaAPIs :: (Client EulerHS.Types.EulerClient API -> MediaAPIs)
mkMediaAPIs mediaClient = (MediaAPIs {..})
  where
    getMediaMediaImage = mediaClient

data MediaUserActionType
  = GET_MEDIA_MEDIA_IMAGE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON MediaUserActionType where
  toJSON GET_MEDIA_MEDIA_IMAGE = Data.Aeson.String "GET_MEDIA_MEDIA_IMAGE"

instance FromJSON MediaUserActionType where
  parseJSON (Data.Aeson.String "GET_MEDIA_MEDIA_IMAGE") = pure GET_MEDIA_MEDIA_IMAGE
  parseJSON _ = fail "GET_MEDIA_MEDIA_IMAGE expected"

$(Data.Singletons.TH.genSingletons [''MediaUserActionType])
