module Domain.Action.Dashboard.AppManagement.TicketDashboard
  ( ticketDashboardUploadAsset,
    ticketDashboardDeleteAsset,
    ticketDashboardCurrentSeatStatus,
    ticketDashboardSeatManagement,
    seatStatus,
  )
where

import API.Types.Dashboard.AppManagement.TicketDashboard (CurrentSeatStatusResp (..), DeletePublicFileRequest (..), UploadPublicFileRequest (..), UploadPublicFileResponse (..))
import qualified API.Types.Dashboard.AppManagement.TicketDashboard
import Data.Maybe
import Domain.Action.Dashboard.AppManagement.EventManagement (getTicketDef)
import Domain.Action.Dashboard.AppManagement.EventManagement.Utils (deleteAssetHelper, uploadAssetHelper)
import qualified "this" Domain.Action.UI.TicketService as ATS
import qualified Domain.Types.EventManagement as DEM
import qualified Domain.Types.Merchant
import qualified Domain.Types.SeatManagement
import qualified "this" Domain.Types.TicketPlace
import qualified Environment
import EulerHS.Prelude hiding (elem, id, length, notElem, whenJust)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.TicketUtils
import qualified Storage.Queries.DraftTicketChange as QDTC
import qualified Storage.Queries.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.SeatManagement as QTSM
import qualified Storage.Queries.ServiceCategory as QSC
import qualified Storage.Queries.TicketPlace as QTP
import Tools.Error

ticketDashboardUploadAsset :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileRequest -> Environment.Flow API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileResponse)
ticketDashboardUploadAsset _merchantShortId _opCity ticketPlaceId (UploadPublicFileRequest {..}) = do
  ticketDef <- getTicketDef ticketPlaceId
  unless (length ticketDef.basicInformation.gallery < 20) $
    throwError $ InvalidRequest "TicketPlace gallery is full, please delete some assets"
  res <- uploadAssetHelper _merchantShortId _opCity ticketPlaceId file reqContentType
  let updatedBasicInfo = ticketDef.basicInformation {DEM.gallery = ticketDef.basicInformation.gallery <> [res.publicUrl]}
  let updatedTicketDef = ticketDef {DEM.basicInformation = updatedBasicInfo, DEM.isDraft = True}
  -- Gallery change goes to the draft; it publishes to the live place on approval.
  QDTC.updateDraftById (Just updatedTicketDef) True ticketPlaceId
  return res

ticketDashboardDeleteAsset :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.TicketDashboard.DeletePublicFileRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
ticketDashboardDeleteAsset _merchantShortId _opCity ticketPlaceId req@(DeletePublicFileRequest {..}) = do
  ticketDef <- getTicketDef ticketPlaceId
  unless (assetId `elem` ticketDef.basicInformation.gallery) $
    throwError $ InvalidRequest "Asset not found in gallery"
  let updatedBasicInfo =
        ticketDef.basicInformation
          { DEM.gallery = filter (/= assetId) ticketDef.basicInformation.gallery
          }
  let updatedTicketDef =
        ticketDef
          { DEM.basicInformation = updatedBasicInfo,
            DEM.isDraft = True
          }
  QDTC.updateDraftById (Just updatedTicketDef) True ticketPlaceId
  mbTicketPlace <- QTP.findById ticketPlaceId
  let liveAssets = fromMaybe [] (mbTicketPlace <&> (.gallery))
  -- Only hard-delete the object if it isn't referenced by the live place; the
  -- live gallery is reconciled when the draft is approved.
  when (assetId `notElem` liveAssets) $ do
    deleteAssetHelper _merchantShortId _opCity req
  return Kernel.Types.APISuccess.Success

ticketDashboardCurrentSeatStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusReq -> Environment.Flow API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusResp)
ticketDashboardCurrentSeatStatus _merchantShortId _opCity _ticketPlaceId req = seatStatus req

ticketDashboardSeatManagement :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.TicketDashboard.SeatManagementReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
ticketDashboardSeatManagement _merchantShortId _opCity _ticketPlaceId req = do
  m <- findMerchantByShortId _merchantShortId
  moCity <- CQMOC.findByMerchantIdAndCity m.id m.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
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
  setupBlockMechanismNx serviceCategoryId visitDate
  -- life is sad
  when (isJust req.maxCapacityChange && isJust req.updateBookedSeats) $ do
    throwError $ InvalidRequest "Cannot change both max capacity and booked seats at the same time"

  whenJust req.maxCapacityChange $ \newMaxCapacity -> do
    ATS.tryChangeMaxCapacity serviceCategoryId newMaxCapacity visitDate

  whenJust req.updateBookedSeats $ \newBookedSeats -> do
    ATS.tryChangeBookedSeats serviceCategoryId newBookedSeats visitDate

  return $ Kernel.Types.APISuccess.Success
