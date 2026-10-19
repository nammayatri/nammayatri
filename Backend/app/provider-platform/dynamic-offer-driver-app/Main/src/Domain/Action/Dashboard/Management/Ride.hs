{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Action.Dashboard.Management.Ride
  ( getRideList,
    getRideAgentList,
    getRideListV2,
    postRideEndMultiple,
    postRideCancelMultiple,
    getRideInfo,
    postRideSync,
    postRideSyncMultiple,
    postRideRoute,
    getRideKaptureList,
    getRideFareBreakUp,
    postRideWaiverRideCancellationPenalty,
    getRideNearby,
    getRideCallCount,
    getRideFlowDebug,
    getRideEvidence,
    getRideEvidenceFile,
    postRideEvidenceVisibility,
  )
where

import qualified "this" API.Types.ProviderPlatform.Management.Ride as Common
import Data.Coerce (coerce)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.Ride as DRide
import qualified Domain.Action.Dashboard.RideFlowDebug as RideFlowDebug
import qualified Domain.Action.UI.Ride.CancelRide as CHandler
import qualified Domain.Action.UI.Ride.EndRide as EHandler
import qualified Domain.Action.UI.RideEvidence as RideEvidence
import qualified Domain.Types.CancellationReason as DCReason
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import Environment
import qualified FileManagement.Domain.Action.UI.File as DFile
import qualified IssueManagement.Storage.Queries.MediaFile as QMediaFile
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.CallStatusExtra as QCallStatus
import qualified Storage.Queries.DriverFee as QDriverFee
import qualified Storage.Queries.QueriesExtra.RideLite as QRideLite
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideEvidence as QRideEvidence
import Tools.Error

getRideList ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Common.BookingStatus ->
  Maybe Currency ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe (Id Common.Driver) ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe HighPrecMoney ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.PaymentMode ->
  Maybe (Id Common.Ride) ->
  Maybe (ShortId Common.Ride) ->
  Maybe UTCTime ->
  Maybe HighPrecMoney ->
  Flow Common.RideListRes
getRideList merchantShortId opCity requestorId mbBookingStatus mbCurrency mbCustomerCountryCode mbCustomerPhone mbDriverCountryCode mbDriverId mbDriverPhone mbFleetOwnerId mbfrom mbFromAmount mbLimit mbOffset mbPaymentMode mbRideId mbReqShortRideId mbto mbToAmount = do
  logInfo $ "Ride list requested by: " <> requestorId
  let customerCountryCode = DCommon.appendPlusInMobileCountryCode mbCustomerCountryCode
      driverCountryCode = DCommon.appendPlusInMobileCountryCode mbDriverCountryCode
  DRide.getRideList merchantShortId opCity mbBookingStatus mbCurrency mbCustomerPhone mbDriverPhone mbfrom mbLimit mbOffset mbPaymentMode mbRideId mbReqShortRideId mbto mbFleetOwnerId mbFromAmount mbToAmount (getId <$> mbDriverId) customerCountryCode driverCountryCode requestorId

getRideAgentList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Common.BookingStatus ->
  Maybe Currency ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe (ShortId Common.Ride) ->
  Maybe UTCTime ->
  Maybe Text ->
  Flow Common.RideListRes
getRideAgentList = DRide.getRideAgentList

postRideEndMultiple :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Common.MultipleRideEndReq -> Flow Common.MultipleRideEndResp
postRideEndMultiple merchantShortId opCity _mbRequestorId req = do
  runRequestValidation Common.validateMultipleRideEndReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  shandle <- EHandler.buildEndRideHandle merchant.id merchantOpCityId Nothing True
  logTagInfo "dashboard -> multipleRideEnd : " $ show (req.rides <&> (.rideId))
  respItems <- forM req.rides $ \reqItem -> do
    info <- handle Common.listItemErrHandler $ do
      let rideId = cast @Common.Ride @DRide.Ride reqItem.rideId
      let cronJobReq =
            EHandler.CronJobEndRideReq
              { point = reqItem.point,
                merchantId = merchant.id
              }
      Success <- EHandler.cronJobEndRide shandle rideId cronJobReq
      pure Common.SuccessItem
    pure $ Common.MultipleRideSyncRespItem {rideId = reqItem.rideId, info}
  pure $ Common.MultipleRideSyncResp {list = respItems}

postRideCancelMultiple :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Common.MultipleRideCancelReq -> Flow Common.MultipleRideCancelResp
postRideCancelMultiple merchantShortId opCity _mbRequestorId req = do
  runRequestValidation Common.validateMultipleRideCancelReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  logTagInfo "dashboard -> multipleRideCancel : " $ show (req.rides <&> (.rideId))
  respItems <- forM req.rides $ \reqItem -> do
    info <- handle Common.listItemErrHandler $ do
      let rideId = cast @Common.Ride @DRide.Ride reqItem.rideId
      let dashboardReq =
            CHandler.CancelRideReq
              { reasonCode = coerce @Common.CancellationReasonCode @DCReason.CancellationReasonCode reqItem.reasonCode,
                additionalInfo = reqItem.additionalInfo,
                doCancellationRateBasedBlocking = Nothing
              }
      Success <- CHandler.dashboardCancelRideHandler CHandler.cancelRideHandle merchant.id merchantOpCityId rideId dashboardReq True
      pure Common.SuccessItem
    pure $ Common.MultipleRideSyncRespItem {rideId = reqItem.rideId, info}
  pure $ Common.MultipleRideSyncResp {list = respItems}

getRideInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Maybe Bool -> Flow Common.RideInfoRes
getRideInfo merchantShortId opCity rideId mbFinanceData = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  DRide.rideInfo merchant.id merchantOpCityId rideId mbFinanceData

postRideSync :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Flow Common.RideSyncRes
postRideSync = DRide.rideSync

postRideSyncMultiple :: ShortId DM.Merchant -> Context.City -> Common.MultipleRideSyncReq -> Flow Common.MultipleRideSyncRes
postRideSyncMultiple = DRide.multipleRideSync

postRideRoute :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Flow Common.RideRouteRes
postRideRoute = DRide.rideRoute

getRideKaptureList :: ShortId DM.Merchant -> Context.City -> Maybe (ShortId Common.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> Flow Common.TicketRideListRes
getRideKaptureList = DRide.ticketRideList

getRideFareBreakUp :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Environment.Flow Common.FareBreakUpRes
getRideFareBreakUp = DRide.fareBreakUp

getRideListV2 :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Currency -> Maybe Text -> Maybe (Id Common.Driver) -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe HighPrecMoney -> Maybe Int -> Maybe Int -> Maybe Common.PaymentMode -> Maybe (Id Common.Ride) -> Maybe (ShortId Common.Ride) -> Maybe Common.RideStatus -> Maybe UTCTime -> Maybe HighPrecMoney -> Flow Common.RideListResV2
getRideListV2 merchantShortId opCity requestorId mbCurrency mbCustomerPhone mbDriverId mbDriverPhone mbFleetOwnerId mbfrom mbFromAmount mbLimit mbOffset mbPaymentMode mbRideId mbReqShortRideId mbRideStatus mbto mbToAmount =
  DRide.getRideListV2 merchantShortId opCity mbCurrency mbCustomerPhone mbDriverId mbDriverPhone mbfrom mbFromAmount mbLimit mbOffset mbPaymentMode mbRideId mbReqShortRideId (castRideStatus <$> mbRideStatus) mbto mbToAmount mbFleetOwnerId requestorId

castRideStatus :: Common.RideStatus -> DRide.RideStatus
castRideStatus s = case s of
  Common.RIDE_UPCOMING -> DRide.UPCOMING
  Common.RIDE_NEW -> DRide.NEW
  Common.RIDE_INPROGRESS -> DRide.INPROGRESS
  Common.RIDE_COMPLETED -> DRide.COMPLETED
  Common.RIDE_CANCELLED -> DRide.CANCELLED

getRideNearby :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.NearbyResp
getRideNearby = DRide.getNearby

getRideCallCount :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Flow Common.RideCallCountRes
getRideCallCount _merchantShortId _opCity rideId = do
  callCount <- QCallStatus.countCallsByEntityId (cast rideId)
  pure $ Common.RideCallCountRes {rideId, callCount}

postRideWaiverRideCancellationPenalty :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Common.WaiverRideCancellationPenaltyReq -> Flow APISuccess
postRideWaiverRideCancellationPenalty _merchantShortId _opCity rideId req = do
  ride <- QRideLite.findByIdLite (cast @Common.Ride @DRide.Ride rideId) >>= fromMaybeM (InvalidRequest $ "Ride does not exist: " <> rideId.getId)
  when (ride.status /= DRide.CANCELLED) $ throwError (InvalidRequest "Ride is not cancelled")
  case (ride.driverCancellationPenaltyFeeId, ride.driverCancellationPenaltyAmount) of
    (Just driverCancellationPenaltyFeeId, Just driverCancellationPenaltyAmount) -> do
      when (isJust ride.driverCancellationPenaltyWaivedReason) $ throwError (InvalidRequest "Driver cancellation penalty amount already waived")
      Redis.whenWithLockRedis (cancellationPenaltyLockKey driverCancellationPenaltyFeeId) 10 $ do
        driverFee <- QDriverFee.findById (Id driverCancellationPenaltyFeeId) >>= fromMaybeM (InvalidRequest $ "Driver cancellation penalty fee does not exist: " <> driverCancellationPenaltyFeeId)
        when (driverFee.status `notElem` [DF.ONGOING, DF.IN_DISPUTE_WINDOW]) $ throwError (InvalidRequest "Driver cancellation penalty fee is not ongoing or in dispute window")
        let newCancellationPenaltyAmount = max 0 $ fromMaybe 0 driverFee.cancellationPenaltyAmount - driverCancellationPenaltyAmount
        now <- getCurrentTime
        QDriverFee.updateCancellationPenaltyAmount (Id driverCancellationPenaltyFeeId) newCancellationPenaltyAmount now
        QRide.updateDriverCancellationPenaltyWaivedReasonAndAmount (Just req.reason) (Just driverCancellationPenaltyAmount) (cast rideId)
    _ -> throwError (InvalidRequest "Driver cancellation penalty fee id or amount is not set")
  pure Success
  where
    cancellationPenaltyLockKey :: Text -> Text
    cancellationPenaltyLockKey id = "Driver:Cancellation:Penalty:DriverFeeId-" <> id

-- After codegen, param order is alphabetical: bapBookingId, bapRideId, bapRideShortId, bapSearchRequestId, bookingId, rideId, rideShortId, searchRequestId
-- BAP IDs are resolved at dashboard level; driver-app ignores them
getRideFlowDebug :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe (Id Common.Ride) -> Maybe (ShortId Common.Ride) -> Maybe Text -> Flow Common.RideFlowDebugRes
getRideFlowDebug merchantShortId opCity _mbBapBookingId _mbBapRideId _mbBapRideShortId _mbBapSearchRequestId mbBookingId mbRideId mbRideShortId mbSearchRequestId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  RideFlowDebug.getRideFlowDebug merchantShortId merchantOpCityId mbRideId mbBookingId mbSearchRequestId mbRideShortId

getRideEvidence :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Flow Common.RideEvidenceListRes
getRideEvidence merchantShortId opCity rideId = do
  ride <- findRideInMerchantCity merchantShortId opCity rideId
  mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = ride.merchantOperatingCityId.getId}) Nothing
  evidences <- QRideEvidence.findAllByRideId ride.id
  mediaFiles <- RideEvidence.loadMediaFiles (evidences >>= \evidence -> evidence.mediaFileId : maybeToList evidence.thumbnailMediaFileId)
  now <- getCurrentTime
  items <-
    forM (sortOn (.createdAt) evidences) $ \evidence -> do
      links <- RideEvidence.signEvidenceLinks (RideEvidence.evidenceLinkExpirySeconds mbTransporterConfig) now mediaFiles evidence
      pure
        Common.RideEvidenceItem
          { id = evidence.id.getId,
            fileId = evidence.mediaFileId.getId,
            driverId = evidence.driverId.getId,
            description = evidence.description,
            rideStatus = castToCommonRideStatus evidence.rideStatus,
            fileStatus = Map.lookup evidence.mediaFileId.getId mediaFiles >>= (.status) <&> show,
            createdAt = evidence.createdAt,
            isHidden = isJust evidence.hiddenAt,
            hiddenAt = evidence.hiddenAt,
            hiddenBy = evidence.hiddenBy,
            hiddenReason = evidence.hiddenReason,
            thumbnailUrl = links.linkToThumbnail,
            imageUrl = links.linkToImage,
            urlsExpireAt = links.linksExpireAt
          }
  pure Common.RideEvidenceListRes {evidence = items}

getRideEvidenceFile :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Text -> Flow Common.RideEvidenceFileRes
getRideEvidenceFile merchantShortId opCity rideId evidenceId = do
  ride <- findRideInMerchantCity merchantShortId opCity rideId
  evidence <- QRideEvidence.findById (Id evidenceId) >>= fromMaybeM (InvalidRequest $ "Evidence does not exist: " <> evidenceId)
  unless (evidence.rideId == ride.id) $ throwError (InvalidRequest $ "Evidence does not exist: " <> evidenceId)
  mediaFile <- QMediaFile.findById evidence.mediaFileId >>= fromMaybeM (InvalidRequest "Evidence file does not exist")
  filePath <- fromMaybeM (InvalidRequest "Evidence file has no stored path") mediaFile.s3FilePath
  content <- DFile.fetchStoredFileOrNotFound filePath
  pure Common.RideEvidenceFileRes {evidenceId, content}

findRideInMerchantCity :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Flow DRide.Ride
findRideInMerchantCity merchantShortId opCity rideId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  ride <- QRide.findById (cast @Common.Ride @DRide.Ride rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.merchantOperatingCityId == merchantOpCityId) $ throwError (RideDoesNotExist rideId.getId)
  pure ride

castToCommonRideStatus :: DRide.RideStatus -> Common.RideStatus
castToCommonRideStatus = \case
  DRide.UPCOMING -> Common.RIDE_UPCOMING
  DRide.NEW -> Common.RIDE_NEW
  DRide.INPROGRESS -> Common.RIDE_INPROGRESS
  DRide.COMPLETED -> Common.RIDE_COMPLETED
  DRide.CANCELLED -> Common.RIDE_CANCELLED

postRideEvidenceVisibility :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Text -> Maybe Text -> Common.SetRideEvidenceVisibilityReq -> Flow APISuccess
postRideEvidenceVisibility merchantShortId opCity rideId evidenceId mbRequestorId req = do
  ride <- findRideInMerchantCity merchantShortId opCity rideId
  evidence <- QRideEvidence.findById (Id evidenceId) >>= fromMaybeM (InvalidRequest $ "Evidence does not exist: " <> evidenceId)
  unless (evidence.rideId == ride.id) $ throwError (InvalidRequest $ "Evidence does not exist: " <> evidenceId)
  let reason = req.reason >>= \r -> let stripped = T.strip r in if T.null stripped then Nothing else Just stripped
  whenJust reason $ \r ->
    when (T.length r > maxHideReasonLength) $
      throwError (InvalidRequest $ "Reason cannot exceed " <> show maxHideReasonLength <> " characters")
  now <- getCurrentTime
  if req.isHidden
    then QRideEvidence.updateVisibility (Just now) mbRequestorId reason evidence.id
    else QRideEvidence.updateVisibility Nothing Nothing Nothing evidence.id
  pure Success

maxHideReasonLength :: Int
maxHideReasonLength = 500
