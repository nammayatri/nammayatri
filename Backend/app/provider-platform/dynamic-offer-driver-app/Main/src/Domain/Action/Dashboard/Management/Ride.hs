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
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Ride as Common
import Data.Coerce (coerce)
import qualified Domain.Action.Dashboard.Ride as DRide
import qualified Domain.Action.UI.Ride.CancelRide as CHandler
import qualified Domain.Action.UI.Ride.EndRide as EHandler
import qualified Domain.Types.CancellationReason as DCReason
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverFee as QDriverFee
import qualified Storage.Queries.Ride as QRide
import Tools.Error

getRideList ::
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
  Flow Common.RideListRes
getRideList = DRide.getRideList

postRideEndMultiple :: ShortId DM.Merchant -> Context.City -> Common.MultipleRideEndReq -> Flow Common.MultipleRideEndResp
postRideEndMultiple merchantShortId opCity req = do
  runRequestValidation Common.validateMultipleRideEndReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  shandle <- EHandler.buildEndRideHandle merchant.id merchantOpCityId Nothing
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

postRideCancelMultiple :: ShortId DM.Merchant -> Context.City -> Common.MultipleRideCancelReq -> Flow Common.MultipleRideCancelResp
postRideCancelMultiple merchantShortId opCity req = do
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
      Success <- CHandler.dashboardCancelRideHandler CHandler.cancelRideHandle merchant.id merchantOpCityId rideId dashboardReq
      pure Common.SuccessItem
    pure $ Common.MultipleRideSyncRespItem {rideId = reqItem.rideId, info}
  pure $ Common.MultipleRideSyncResp {list = respItems}

getRideInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Flow Common.RideInfoRes
getRideInfo merchantShortId opCity rideId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  DRide.rideInfo merchant.id merchantOpCityId rideId

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

getRideListV2 :: ShortId DM.Merchant -> Context.City -> Maybe Currency -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Int -> Maybe Int -> Maybe (ShortId Common.Ride) -> Maybe Common.RideStatus -> Maybe UTCTime -> Flow Common.RideListResV2
getRideListV2 merchantShortId opCity mbCurrency mbCustomerPhone mbDriverPhone mbfrom mbLimit mbOffset mbReqShortRideId mbRideStatus mbto =
  DRide.getRideListV2 merchantShortId opCity mbCurrency mbCustomerPhone mbDriverPhone mbfrom mbLimit mbOffset mbReqShortRideId (castRideStatus <$> mbRideStatus) mbto

castRideStatus :: Common.RideStatus -> DRide.RideStatus
castRideStatus s = case s of
  Common.RIDE_UPCOMING -> DRide.UPCOMING
  Common.RIDE_NEW -> DRide.NEW
  Common.RIDE_INPROGRESS -> DRide.INPROGRESS
  Common.RIDE_COMPLETED -> DRide.COMPLETED
  Common.RIDE_CANCELLED -> DRide.CANCELLED

postRideWaiverRideCancellationPenalty :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Common.WaiverRideCancellationPenaltyReq -> Flow APISuccess
postRideWaiverRideCancellationPenalty _merchantShortId _opCity rideId req = do
  ride <- QRide.findById (cast @Common.Ride @DRide.Ride rideId) >>= fromMaybeM (InvalidRequest $ "Ride does not exist: " <> rideId.getId)
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
