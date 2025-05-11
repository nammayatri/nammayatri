{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.TicketDashboard where

import qualified AWS.S3
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.MerchantOnboarding
import qualified "this" Domain.Types.TicketPlace
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data DeletePublicFileRequest = DeletePublicFileRequest {assetId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UploadPublicFileRequest = UploadPublicFileRequest {file :: Kernel.Prelude.FilePath, reqContentType :: Kernel.Prelude.Text, fileType :: AWS.S3.FileType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UploadPublicFileRequest where
  hideSecrets = Kernel.Prelude.identity

data UploadPublicFileResponse = UploadPublicFileResponse {publicUrl :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = (TicketDashboardUploadAsset :<|> TicketDashboardDeleteAsset)

type TicketDashboardUploadAsset =
  ( "ticketdashboard" :> "ticketplace" :> Capture "ticketPlaceId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "uploadAsset"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           UploadPublicFileRequest
      :> Post
           '[JSON]
           UploadPublicFileResponse
  )

type TicketDashboardDeleteAsset =
  ( "ticketdashboard" :> "ticketplace" :> Capture "ticketPlaceId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "deleteAsset"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody
           '[JSON]
           DeletePublicFileRequest
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data TicketDashboardAPIs = TicketDashboardAPIs
  { ticketDashboardUploadAsset ::
      Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
      Kernel.Prelude.Maybe Kernel.Prelude.Text ->
      Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole ->
      ( Data.ByteString.Lazy.ByteString,
        UploadPublicFileRequest
      ) ->
      EulerHS.Types.EulerClient UploadPublicFileResponse,
    ticketDashboardDeleteAsset :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> DeletePublicFileRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkTicketDashboardAPIs :: (Client EulerHS.Types.EulerClient API -> TicketDashboardAPIs)
mkTicketDashboardAPIs ticketDashboardClient = (TicketDashboardAPIs {..})
  where
    ticketDashboardUploadAsset :<|> ticketDashboardDeleteAsset = ticketDashboardClient

data TicketDashboardUserActionType
  = TICKET_DASHBOARD_UPLOAD_ASSET
  | TICKET_DASHBOARD_DELETE_ASSET
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''TicketDashboardUserActionType])
