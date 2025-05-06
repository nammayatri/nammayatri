{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.TicketDashboard
  ( ticketDashboardUploadAsset,
    ticketDashboardDeleteAsset,
  )
where

import API.Types.Dashboard.AppManagement.TicketDashboard (UploadPublicFileRequest (..), UploadPublicFileResponse (..))
import qualified API.Types.Dashboard.AppManagement.TicketDashboard
import qualified AWS.S3 as S3
import qualified Data.ByteString as BS
import Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.MerchantOnboarding
import qualified "this" Domain.Types.TicketPlace
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, length)
import EulerHS.Types (base64Encode)
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.RiderConfig as QRC
import qualified Storage.Queries.TicketPlace as QTP
import System.IO (IOMode (ReadMode), hFileSize)
import Tools.Auth
import Tools.Error

ticketDashboardUploadAsset :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileRequest -> Environment.Flow API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileResponse)
ticketDashboardUploadAsset _merchantShortId _opCity ticketPlaceId requestorId _mbRequestorRole (UploadPublicFileRequest {..}) = do
  _reqId <- fromMaybeM (InvalidRequest "RequestorId is required") requestorId
  reqRole <- _mbRequestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  ticketPlace <- QTP.findById ticketPlaceId >>= fromMaybeM (InvalidRequest "TicketPlace not found")
  unless ((reqRole == Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_ADMIN || (reqRole == Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_MERCHANT) && ticketPlace.ticketMerchantId == requestorId)) $ throwError $ InvalidRequest "Requestor does not have this access"
  unless (length ticketPlace.gallery <= 20) $ throwError $ InvalidRequest "TicketPlace gallery is full, please delete some assets"
  fileSize <- L.runIO $ withFile file ReadMode hFileSize
  when (fileSize > 1000000) $
    throwError $ FileSizeExceededError ("File size " <> show fileSize <> " exceeds the limit of 1 MB")
  documentFile <- L.runIO $ base64Encode <$> BS.readFile file
  uuid <- generateGUID
  m <- findMerchantByShortId _merchantShortId
  moCity <- CQMOC.findByMerchantIdAndCity m.id m.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  riderConfig <- QRC.findByMerchantOperatingCityId moCity.id >>= fromMaybeM (RiderConfigNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  filePath <- S3.createFilePublicPath "ticket-assets" ("ticket-place/" <> ticketPlaceId.getId) uuid reqContentType
  _ <- fork "S3 put file" $ S3.putPublic (T.unpack filePath) documentFile
  let url = fromMaybe mempty (riderConfig.ticketAssetDomain) <> "/" <> filePath
  QTP.updateGalleryById (ticketPlace.gallery <> [url]) ticketPlaceId
  return $ UploadPublicFileResponse {publicUrl = url}

ticketDashboardDeleteAsset :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
ticketDashboardDeleteAsset _merchantShortId _opCity ticketPlaceId assetId requestorId requestorRole = do
  _reqId <- fromMaybeM (InvalidRequest "RequestorId is required") requestorId
  reqRole <- requestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  ticketPlace <- QTP.findById ticketPlaceId >>= fromMaybeM (InvalidRequest "TicketPlace not found")
  unless ((reqRole == Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_ADMIN || (reqRole == Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_MERCHANT) && ticketPlace.ticketMerchantId == requestorId)) $ throwError $ InvalidRequest "Requestor does not have this access"
  m <- findMerchantByShortId _merchantShortId
  moCity <- CQMOC.findByMerchantIdAndCity m.id m.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  riderConfig <- QRC.findByMerchantOperatingCityId moCity.id >>= fromMaybeM (RiderConfigNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  let filePath = fromMaybe assetId $ T.stripPrefix (fromMaybe mempty (riderConfig.ticketAssetDomain) <> "/") assetId
  _ <- fork "S3 delete file" $ S3.deletePublic (T.unpack filePath)
  QTP.updateGalleryById (filter (\x -> x /= assetId) ticketPlace.gallery) ticketPlaceId
  return $ Kernel.Types.APISuccess.Success
