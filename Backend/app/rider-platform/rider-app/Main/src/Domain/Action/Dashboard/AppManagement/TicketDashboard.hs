{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.TicketDashboard
  ( ticketDashboardUploadAsset,
    ticketDashboardDeleteAsset,
    ticketDashboardCurrentSeatStatus,
    ticketDashboardSeatManagement,
  )
where

import API.Types.Dashboard.AppManagement.TicketDashboard (CurrentSeatStatusResp (..), UploadPublicFileRequest (..), UploadPublicFileResponse (..))
import qualified API.Types.Dashboard.AppManagement.TicketDashboard
import qualified AWS.S3 as S3
import qualified Data.ByteString as BS
import Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified "this" Domain.Action.UI.TicketService as ATS
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.MerchantOnboarding
import qualified Domain.Types.SeatManagement
import qualified "this" Domain.Types.TicketPlace
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, length, whenJust)
import EulerHS.Types (base64Encode)
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.RiderConfig as QRC
import qualified Storage.Queries.SeatManagement as QTSM
import qualified Storage.Queries.ServiceCategory as QSC
import qualified Storage.Queries.TicketPlace as QTP
import System.FilePath (takeExtension)
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
  fileExtension <- getExtension reqContentType
  when (fileSize > 1000000) $
    throwError $ FileSizeExceededError ("File size " <> show fileSize <> " exceeds the limit of 1 MB")
  documentFile <- L.runIO $ BS.readFile file
  uuid <- generateGUID
  m <- findMerchantByShortId _merchantShortId
  moCity <- CQMOC.findByMerchantIdAndCity m.id m.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  riderConfig <- QRC.findByMerchantOperatingCityId moCity.id >>= fromMaybeM (RiderConfigNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  filePath <- S3.createFilePublicPath "ticket-assets" ("ticket-place/" <> ticketPlaceId.getId) uuid fileExtension
  logDebug $ "filePath: " <> show filePath
  _ <- fork "S3 put file" $ S3.putPublicRaw (T.unpack filePath) documentFile (T.unpack reqContentType)
  let url = fromMaybe mempty (riderConfig.ticketAssetDomain) <> filePath
  QTP.updateGalleryById (ticketPlace.gallery <> [url]) ticketPlaceId
  return $ UploadPublicFileResponse {publicUrl = url}

ticketDashboardDeleteAsset :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> API.Types.Dashboard.AppManagement.TicketDashboard.DeletePublicFileRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
ticketDashboardDeleteAsset _merchantShortId _opCity ticketPlaceId requestorId requestorRole req = do
  _reqId <- fromMaybeM (InvalidRequest "RequestorId is required") requestorId
  reqRole <- requestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  ticketPlace <- QTP.findById ticketPlaceId >>= fromMaybeM (InvalidRequest "TicketPlace not found")
  unless ((reqRole == Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_ADMIN || (reqRole == Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_MERCHANT) && ticketPlace.ticketMerchantId == requestorId)) $ throwError $ InvalidRequest "Requestor does not have this access"
  m <- findMerchantByShortId _merchantShortId
  moCity <- CQMOC.findByMerchantIdAndCity m.id m.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  riderConfig <- QRC.findByMerchantOperatingCityId moCity.id >>= fromMaybeM (RiderConfigNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  let filePath = fromMaybe req.assetId $ T.stripPrefix (fromMaybe mempty (riderConfig.ticketAssetDomain)) req.assetId
  _ <- fork "S3 delete file" $ S3.deletePublic (T.unpack filePath)
  QTP.updateGalleryById (filter (\x -> x /= req.assetId) ticketPlace.gallery) ticketPlaceId
  return $ Kernel.Types.APISuccess.Success

getExtension :: Text -> Environment.Flow Text
getExtension = \case
  "image/jpeg" -> return ".jpg"
  "image/png" -> return ".png"
  _ -> throwError $ InvalidRequest "Invalid file type"

ticketDashboardCurrentSeatStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusReq -> Environment.Flow API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusResp)
ticketDashboardCurrentSeatStatus _merchantShortId _opCity ticketPlaceId requestorId requestorRole_ req = do
  _reqId <- fromMaybeM (InvalidRequest "RequestorId is required") requestorId
  reqRole <- requestorRole_ & fromMaybeM (InvalidRequest "RequestorRole is required")
  ticketPlace <- QTP.findById ticketPlaceId >>= fromMaybeM (InvalidRequest "TicketPlace not found")
  unless (reqRole == Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_ADMIN || (reqRole == Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_MERCHANT && ticketPlace.ticketMerchantId == requestorId)) $ throwError $ InvalidRequest "Requestor does not have this access"
  let serviceCategoryId = Kernel.Types.Id.Id req.serviceCategory
  let visitDate = req.date
  ATS.setupBlockMechanismNx serviceCategoryId visitDate
  mbBookedCount :: Maybe Int <- Redis.get (ATS.mkTicketServiceCategoryBookedCountKey serviceCategoryId visitDate)
  mbAllowedMaxCapacity :: Maybe Int <- Redis.get (ATS.mkTicketServiceAllowedMaxCapacityKey serviceCategoryId visitDate)
  if (isNothing mbBookedCount && isNothing mbAllowedMaxCapacity)
    then do
      mbSeatM <- QTSM.findByTicketServiceCategoryIdAndDate serviceCategoryId visitDate
      let bookedCount = maybe 0 (.booked) mbSeatM
      return $ CurrentSeatStatusResp {maxCapacity = Nothing, remainingCapacity = Nothing, bookedCount = bookedCount, unlimitedCapacity = True}
    else do
      bookedCount <- mbBookedCount & fromMaybeM (InternalError "Booked count not found")
      maxCapacity <- mbAllowedMaxCapacity & fromMaybeM (InternalError "Max capacity not found")
      return $ CurrentSeatStatusResp {maxCapacity = Just maxCapacity, remainingCapacity = Just (maxCapacity - bookedCount), bookedCount = bookedCount, unlimitedCapacity = False}

ticketDashboardSeatManagement :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> API.Types.Dashboard.AppManagement.TicketDashboard.SeatManagementReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
ticketDashboardSeatManagement _merchantShortId _opCity ticketPlaceId requestorId requestorRole_ req = do
  m <- findMerchantByShortId _merchantShortId
  moCity <- CQMOC.findByMerchantIdAndCity m.id m.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  _reqId <- fromMaybeM (InvalidRequest "RequestorId is required") requestorId
  reqRole <- requestorRole_ & fromMaybeM (InvalidRequest "RequestorRole is required")
  ticketPlace <- QTP.findById ticketPlaceId >>= fromMaybeM (InvalidRequest "TicketPlace not found")
  unless (reqRole == Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_ADMIN || (reqRole == Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_MERCHANT && ticketPlace.ticketMerchantId == requestorId)) $ throwError $ InvalidRequest "Requestor does not have this access"
  let serviceCategoryId = Kernel.Types.Id.Id req.serviceCategory
  let visitDate = req.date
  tBookingSC <- QSC.findById serviceCategoryId >>= fromMaybeM (InvalidRequest "TicketBookingServiceCategory not found")
  mbSeatM <- QTSM.findByTicketServiceCategoryIdAndDate serviceCategoryId visitDate
  when (isNothing mbSeatM) $ do
    seatId <- generateGUID
    now <- getCurrentTime
    let seatM =
          Domain.Types.SeatManagement.SeatManagement
            { id = seatId,
              ticketServiceCategoryId = serviceCategoryId,
              date = visitDate,
              blocked = 0,
              booked = 0,
              maxCapacity = tBookingSC.availableSeats,
              merchantId = Just m.id,
              merchantOperatingCityId = Just moCity.id,
              createdAt = now,
              updatedAt = now
            }
    QTSM.create seatM
  ATS.setupBlockMechanismNx serviceCategoryId visitDate
  -- life is sad
  when (isJust req.maxCapacityChange && isJust req.updateBookedSeats) $ do
    throwError $ InvalidRequest "Cannot change both max capacity and booked seats at the same time"

  whenJust req.maxCapacityChange $ \newMaxCapacity -> do
    ATS.tryChangeMaxCapacity serviceCategoryId newMaxCapacity visitDate

  whenJust req.updateBookedSeats $ \newBookedSeats -> do
    ATS.tryChangeBookedSeats serviceCategoryId newBookedSeats visitDate

  return $ Kernel.Types.APISuccess.Success
