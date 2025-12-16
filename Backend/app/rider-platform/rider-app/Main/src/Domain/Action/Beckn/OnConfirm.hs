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
import qualified Domain.Types.BookingStatus as DRB
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id, whenJust)
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.External.Maps (LatLong (..))
import Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import Tools.Metrics (HasBAPMetrics)
import qualified Tools.SMS as Sms
import TransactionLogs.Types
import qualified UrlShortner.Common as UrlShortner

data OnConfirmReq
  = RideAssigned RideAssignedInfo
  | BookingConfirmed BookingConfirmedInfo

data BookingConfirmedInfo = BookingConfirmedInfo
  { bppBookingId :: Id DRB.BPPBooking,
    specialZoneOtp :: Maybe Text,
    fareBreakups :: [DCommon.DFareBreakup]
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
    driverAlternatePhoneNumber :: Maybe Text,
    isFreeRide :: Bool,
    previousRideEndPos :: Maybe LatLong,
    rideOtp :: Text,
    vehicleNumber :: Text,
    vehicleColor :: Maybe Text,
    vehicleModel :: Text,
    fareBreakups :: Maybe [DCommon.DFareBreakup],
    isAlreadyFav :: Bool,
    favCount :: Maybe Int,
    driverAccountId :: Maybe Payment.AccountId,
    isSafetyPlus :: Bool
  }

data ValidatedOnConfirmReq
  = ValidatedRideAssigned DCommon.ValidatedRideAssignedReq
  | ValidatedBookingConfirmed ValidatedBookingConfirmedReq

data ValidatedBookingConfirmedReq = ValidatedBookingConfirmedReq
  { bppBookingId :: Id DRB.BPPBooking,
    specialZoneOtp :: Maybe Text,
    booking :: DRB.Booking,
    fareBreakups :: [DCommon.DFareBreakup]
  }

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
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "storeRidesTimeLimit" r Int,
    HasBAPMetrics m r,
    EventStreamFlow m r
  ) =>
  ValidatedOnConfirmReq ->
  m ()
onConfirm (ValidatedBookingConfirmed ValidatedBookingConfirmedReq {..}) = do
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
            let countryCode = fromMaybe "+91" customer.mobileCountryCode
            let phoneNumber = countryCode <> mobileNumber
            buildSmsReq <-
              MessageBuilder.buildSendBookingOTPMessage merchantOperatingCityId customer.language $
                MessageBuilder.BuildSendBookingOTPMessageReq
                  { otp = show otp,
                    amount = show (booking.estimatedTotalFare.amountInt)
                  }
            Sms.sendSMS booking.merchantId merchantOperatingCityId (buildSmsReq phoneNumber) >>= Sms.checkSmsResult
          else do
            logInfo "Merchant not configured to send dashboard sms"
  case booking.bookingDetails of
    DRB.DriverOfferDetails _ -> return ()
    _ -> void $ QRB.updateStatus booking.id DRB.CONFIRMED
-- TODO: Find a Better way to remove this from on_confirm.
-- void $ QRB.updateStatus booking.id DRB.CONFIRMED
onConfirm (ValidatedRideAssigned req) = DCommon.rideAssignedReqHandler req

-- TODO: Make sure booking status is new here.
validateRequest :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, EsqDBReplicaFlow m r, HasHttpClientOptions r c, HasLongDurationRetryCfg r c, HasField "minTripDistanceForReferralCfg" r (Maybe Distance)) => OnConfirmReq -> Text -> Bool -> m ValidatedOnConfirmReq
validateRequest (BookingConfirmed BookingConfirmedInfo {..}) _txnId _isValueAddNP = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId-" <> bppBookingId.getId)
  return $ ValidatedBookingConfirmed ValidatedBookingConfirmedReq {..}
validateRequest (RideAssigned RideAssignedInfo {..}) transactionId isValueAddNP = do
  let bookingDetails = DCommon.BookingDetails {otp = rideOtp, isInitiatedByCronJob = False, ..}
  booking <-
    if isValueAddNP
      then QRB.findByBPPBookingId bppBookingId |<|>| QRB.findByTransactionId transactionId >>= fromMaybeM (BookingDoesNotExist $ "transactionId:-" <> transactionId)
      else QRB.findByTransactionId transactionId >>= fromMaybeM (BookingDoesNotExist $ "transactionId:-" <> transactionId)
  mbMerchant <- CQM.findById booking.merchantId
  let onlinePayment = SPayment.isOnlinePayment mbMerchant booking
  onlinePaymentParameters <-
    if onlinePayment
      then do
        person <- runInReplica $ QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
        (customerPaymentId, paymentMethodId) <- SPayment.getCustomerAndPaymentMethod booking person
        driverAccountId_ <- driverAccountId & fromMaybeM (DriverAccountIdNotFound booking.id.getId)
        let merchantOperatingCityId = person.merchantOperatingCityId
        email <- mapM decrypt person.email
        return $ Just DCommon.OnlinePaymentParameters {driverAccountId = driverAccountId_, ..}
      else return Nothing
  return $ ValidatedRideAssigned DCommon.ValidatedRideAssignedReq {onlinePaymentParameters, driverTrackingUrl = Nothing, ..}
