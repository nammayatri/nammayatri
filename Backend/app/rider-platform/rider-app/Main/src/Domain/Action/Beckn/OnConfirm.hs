{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnConfirm
  ( onConfirm,
    validateRequest,
    OnConfirmReq (..),
    RideAssignedInfo (..),
    BookingConfirmedInfo (..),
    ValidatedOnConfirmReq (..),
    ValidatedBookingConfirmedReq (..),
    DCommon.RideAssignedReq (..),
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.Beckn.Common as DCommon
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.External.Maps (LatLong (..))
import Kernel.External.Types (SchedulerFlow)
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import Tools.Metrics (HasBAPMetrics)
import qualified Tools.SMS as Sms
import TransactionLogs.Types

data OnConfirmReq
  = RideAssigned RideAssignedInfo
  | BookingConfirmed BookingConfirmedInfo

data BookingConfirmedInfo = BookingConfirmedInfo
  { bppBookingId :: Id DRB.BPPBooking,
    specialZoneOtp :: Maybe Text,
    fareParams :: Maybe [DCommon.DFareBreakup]
  }

data RideAssignedInfo = RideAssignedInfo
  { bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    driverName :: Text,
    driverImage :: Maybe Text,
    driverMobileNumber :: Text,
    driverMobileCountryCode :: Maybe Text,
    driverRating :: Maybe Centesimal,
    driverRegisteredAt :: Maybe UTCTime,
    isDriverBirthDay :: Bool,
    vehicleAge :: Maybe Months,
    isFreeRide :: Bool,
    previousRideEndPos :: Maybe LatLong,
    rideOtp :: Text,
    vehicleNumber :: Text,
    vehicleColor :: Maybe Text,
    vehicleModel :: Text,
    fareParams :: Maybe [DCommon.DFareBreakup]
  }

data ValidatedOnConfirmReq
  = ValidatedRideAssigned DCommon.ValidatedRideAssignedReq
  | ValidatedBookingConfirmed ValidatedBookingConfirmedReq

data ValidatedBookingConfirmedReq = ValidatedBookingConfirmedReq
  { bppBookingId :: Id DRB.BPPBooking,
    specialZoneOtp :: Maybe Text,
    booking :: DRB.Booking,
    fareParams :: Maybe [DCommon.DFareBreakup]
  }

createFareBreakup ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasField "storeRidesTimeLimit" r Int,
    HasBAPMetrics m r,
    EventStreamFlow m r
  ) =>
  Id DRB.Booking ->
  Maybe [DCommon.DFareBreakup] ->
  m ()
createFareBreakup booking fareParams' = do
  let fareParamsList = fromMaybe [] fareParams'
  fareBreakups' <- traverse (DCommon.buildFareBreakupV2 booking.getId DFareBreakup.INITIAL_BOOKING) fareParamsList
  fareBreakups <- traverse (DCommon.buildFareBreakupV2 booking.getId DFareBreakup.BOOKING) fareParamsList
  QFareBreakup.createMany fareBreakups
  QFareBreakup.createMany fareBreakups'

onConfirm ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasField "storeRidesTimeLimit" r Int,
    HasBAPMetrics m r,
    EventStreamFlow m r
  ) =>
  ValidatedOnConfirmReq ->
  m ()
onConfirm (ValidatedBookingConfirmed ValidatedBookingConfirmedReq {..}) = do
  createFareBreakup booking.id fareParams
  whenJust specialZoneOtp $ \otp -> do
    void $ QRB.updateOtpCodeBookingId booking.id otp
    when (booking.isDashboardRequest == Just True) $
      fork "sending Booking confirmed dasboard sms" $ do
        let merchantOperatingCityId = booking.merchantOperatingCityId
        merchantConfig <- QMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
        if merchantConfig.enableDashboardSms
          then do
            customer <- B.runInReplica $ QPerson.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
            mobileNumber <- mapM decrypt customer.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
            smsCfg <- asks (.smsCfg)
            let countryCode = fromMaybe "+91" customer.mobileCountryCode
            let phoneNumber = countryCode <> mobileNumber
                sender = smsCfg.sender
            message <-
              MessageBuilder.buildSendBookingOTPMessage merchantOperatingCityId $
                MessageBuilder.BuildSendBookingOTPMessageReq
                  { otp = show otp,
                    amount = show (booking.estimatedTotalFare.amountInt)
                  }
            Sms.sendSMS booking.merchantId merchantOperatingCityId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
          else do
            logInfo "Merchant not configured to send dashboard sms"
  case booking.bookingDetails of
    DRB.DriverOfferDetails _ -> return ()
    _ -> void $ QRB.updateStatus booking.id DRB.CONFIRMED
-- TODO: Find a Better way to remove this from on_confirm.
-- void $ QRB.updateStatus booking.id DRB.CONFIRMED
onConfirm (ValidatedRideAssigned DCommon.ValidatedRideAssignedReq {..}) = do
  createFareBreakup booking.id fareBreakups
  DCommon.rideAssignedReqHandler DCommon.ValidatedRideAssignedReq {..}

-- TODO: Make sure booking status is new here.
validateRequest :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => OnConfirmReq -> Text -> m ValidatedOnConfirmReq
validateRequest (BookingConfirmed BookingConfirmedInfo {..}) _txnId = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId-" <> bppBookingId.getId)
  return $ ValidatedBookingConfirmed ValidatedBookingConfirmedReq {..}
validateRequest (RideAssigned RideAssignedInfo {..}) transactionId = do
  let bookingDetails = DCommon.BookingDetails {otp = rideOtp, isInitiatedByCronJob = False, ..}
  booking <- QRB.findByTransactionId transactionId >>= fromMaybeM (BookingDoesNotExist $ "transactionId:-" <> transactionId)
  return $ ValidatedRideAssigned DCommon.ValidatedRideAssignedReq {onlinePaymentParameters = Nothing, fareBreakups = Nothing, driverTrackingUrl = Nothing, ..}
