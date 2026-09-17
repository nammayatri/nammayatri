{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.TicketDashboard where

import qualified AWS.S3
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time.Calendar
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

data CurrentSeatStatusReq = CurrentSeatStatusReq {serviceCategory :: Kernel.Prelude.Text, date :: Data.Time.Calendar.Day}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CurrentSeatStatusResp = CurrentSeatStatusResp
  { maxCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    remainingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bookedCount :: Kernel.Prelude.Int,
    unlimitedCapacity :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DeletePublicFileRequest = DeletePublicFileRequest {assetId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SeatManagementReq = SeatManagementReq
  { serviceCategory :: Kernel.Prelude.Text,
    date :: Data.Time.Calendar.Day,
    maxCapacityChange :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    updateBookedSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
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

type API = (TicketDashboardUploadAsset :<|> TicketDashboardDeleteAsset :<|> TicketDashboardCurrentSeatStatus :<|> TicketDashboardSeatManagement)

type TicketDashboardUploadAsset =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "uploadAsset"
      :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp UploadPublicFileRequest
      :> Post
           '[JSON]
           UploadPublicFileResponse
  )

type TicketDashboardDeleteAsset =
  ( "ticketdashboard" :> "ticketplace" :> Capture "ticketPlaceId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "deleteAsset"
      :> ReqBody
           '[JSON]
           DeletePublicFileRequest
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type TicketDashboardCurrentSeatStatus =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "currentSeatStatus"
      :> ReqBody '[JSON] CurrentSeatStatusReq
      :> Post '[JSON] CurrentSeatStatusResp
  )

type TicketDashboardSeatManagement =
  ( "ticketdashboard" :> "ticketplace" :> Capture "ticketPlaceId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "seatManagement"
      :> ReqBody
           '[JSON]
           SeatManagementReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

data TicketDashboardAPIs = TicketDashboardAPIs
  { ticketDashboardUploadAsset ::
      Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
      ( Data.ByteString.Lazy.ByteString,
        UploadPublicFileRequest
      ) ->
      EulerHS.Types.EulerClient UploadPublicFileResponse,
    ticketDashboardDeleteAsset :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> DeletePublicFileRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    ticketDashboardCurrentSeatStatus :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> CurrentSeatStatusReq -> EulerHS.Types.EulerClient CurrentSeatStatusResp,
    ticketDashboardSeatManagement :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> SeatManagementReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkTicketDashboardAPIs :: (Client EulerHS.Types.EulerClient API -> TicketDashboardAPIs)
mkTicketDashboardAPIs ticketDashboardClient = (TicketDashboardAPIs {..})
  where
    ticketDashboardUploadAsset :<|> ticketDashboardDeleteAsset :<|> ticketDashboardCurrentSeatStatus :<|> ticketDashboardSeatManagement = ticketDashboardClient

data TicketDashboardUserActionType
  = TICKET_DASHBOARD_UPLOAD_ASSET
  | TICKET_DASHBOARD_DELETE_ASSET
  | TICKET_DASHBOARD_CURRENT_SEAT_STATUS
  | TICKET_DASHBOARD_SEAT_MANAGEMENT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''TicketDashboardUserActionType])
