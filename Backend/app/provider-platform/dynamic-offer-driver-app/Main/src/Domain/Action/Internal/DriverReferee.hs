{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.DriverReferee where

import Data.Fixed (div')
import Data.OpenApi (ToSchema)
import Data.Time (utctDay)
import qualified Domain.Action.UI.Payout as DAP
import qualified Domain.Types.DailyStats as DDS
import qualified Domain.Types.DriverReferral as Domain
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.RefereeLink as DRL
import qualified Domain.Types.RiderDetails as DRD
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (encrypt, getDbHash)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Text as TU
import qualified SharedLogic.Merchant as SMerchant
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DriverInformation as DI
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Notifications
import Utils.Common.CacUtils

data RefereeLinkInfoReq = RefereeLinkInfoReq
  { referralCode :: Id Domain.DriverReferral,
    customerMobileNumber :: Text,
    customerMobileCountryCode :: Text,
    isMultipleDeviceIdExist :: Maybe Bool,
    alreadyReferred :: Maybe Bool,
    shareReferrerInfo :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

linkReferee ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Id Merchant ->
  Maybe Text ->
  RefereeLinkInfoReq ->
  m DRL.LinkRefereeRes
linkReferee merchantId apiKey RefereeLinkInfoReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  unless (TU.validateAllDigitWithMinLength 6 referralCode.getId) $
    throwError $ InvalidRequest "Referral Code must have 6 digits"
  driverReferralLinkage <- QDR.findByRefferalCode referralCode >>= fromMaybeM (InvalidRequest "Invalid referral code.")
  driverInfo <- DI.findById driverReferralLinkage.driverId >>= fromMaybeM (PersonNotFound driverReferralLinkage.driverId.getId)
  unless (driverInfo.enabled) $
    throwError $ InvalidRequest "Driver is not enabled"
  driver <- QP.findById driverReferralLinkage.driverId >>= fromMaybeM (PersonNotFound driverReferralLinkage.driverId.getId)
  driverStats <- QDriverStats.findByPrimaryKey driver.id >>= fromMaybeM (PersonNotFound driver.id.getId)
  let merchOpCityId = driver.merchantOperatingCityId
      isMultipleDeviceIdExist_ = fromMaybe False isMultipleDeviceIdExist
  currency <- SMerchant.getCurrencyByMerchantOpCity merchOpCityId
  transporterConfig <- SCTC.findByMerchantOpCityId merchOpCityId (Just (DriverId (cast driverReferralLinkage.driverId))) >>= fromMaybeM (TransporterConfigNotFound merchOpCityId.getId)
  let alreadyReferred_ = fromMaybe False alreadyReferred
  unless alreadyReferred_ $ do
    updateReferralStats driverReferralLinkage.driverId driverStats transporterConfig merchOpCityId
    numberHash <- getDbHash customerMobileNumber
    updateRefereeInfoAndNotify transporterConfig isMultipleDeviceIdExist_ numberHash driverReferralLinkage currency driver merchOpCityId
  let shareDriverInfo = fromMaybe False shareReferrerInfo
  res <-
    if not shareDriverInfo
      then pure $ Left Success
      else do
        vehicle <- QVeh.findById driver.id >>= fromMaybeM (VehicleNotFound $ "driverId:-" <> driver.id.getId)
        let rating =
              liftA2 (,) driverStats.totalRatingScore driverStats.totalRatings
                >>= \(totalRatingScore, totalRatings) ->
                  bool Nothing (Just . toCentesimal $ div' totalRatingScore totalRatings) (totalRatings > 0)
            info =
              DRL.ReferrerInfo
                { firstName = driver.firstName,
                  middleName = driver.middleName,
                  lastName = driver.lastName,
                  rating,
                  registeredAt = driver.createdAt,
                  totalRides = driverStats.totalRides,
                  vehicleNumber = vehicle.registrationNo,
                  vehicleVariant = show vehicle.variant,
                  applicableServiceTiers = show <$> vehicle.selectedServiceTiers,
                  driverImage = Nothing
                }
        pure $ Right info
  pure $ DRL.LinkRefereeRes res
  where
    mkRiderDetailsObj driverId currency flagReason merchOpCityId = do
      id <- generateGUID
      now <- getCurrentTime
      otp <- generateOTPCode
      encPhoneNumber <- encrypt customerMobileNumber
      pure $
        DRD.RiderDetails
          { id = Id id,
            mobileCountryCode = customerMobileCountryCode,
            mobileNumber = encPhoneNumber,
            merchantId,
            createdAt = now,
            updatedAt = now,
            referralCode = Just referralCode,
            referredByDriver = Just driverId,
            referredAt = Just now,
            hasTakenValidRide = False,
            hasTakenValidRideAt = Nothing,
            otpCode = Just otp,
            nightSafetyChecks = True,
            cancellationDues = 0.0,
            disputeChancesUsed = 0,
            firstRideId = Nothing,
            payoutFlagReason = flagReason,
            currency,
            isDeviceIdExists = Just $ isJust isMultipleDeviceIdExist,
            isFlagConfirmed = Nothing,
            merchantOperatingCityId = Just merchOpCityId
          }

    updateRefereeInfoAndNotify transporterConfig isMultipleDeviceIdExist_ numberHash driverReferralLinkage currency driver merchOpCityId = do
      let isDeviceIdChecksRequired = fromMaybe False transporterConfig.isDeviceIdChecksRequired
          flagReason
            | isDeviceIdChecksRequired = Nothing
            | isMultipleDeviceIdExist_ = Just DRD.MultipleDeviceIdExists
            | otherwise = Nothing
      mbRiderDetails <- QRD.findByMobileNumberHashAndMerchant numberHash merchantId
      _ <- case mbRiderDetails of
        Just rd -> do
          QRD.updateReferralInfo numberHash merchantId referralCode driverReferralLinkage.driverId
          QRD.updateIsDeviceIdExists (Just $ isJust isMultipleDeviceIdExist) rd.id
        Nothing -> do
          riderDetails <- mkRiderDetailsObj driverReferralLinkage.driverId currency flagReason merchOpCityId
          QRD.create riderDetails
      mbMerchantPN <- CPN.findMatchingMerchantPN driver.merchantOperatingCityId "REFERRAL_FLOW" Nothing Nothing driver.language Nothing
      whenJust mbMerchantPN $ \merchantPN -> do
        let entityData = NotifReq {entityId = driver.id.getId, title = merchantPN.title, message = merchantPN.body}
        notifyDriverOnEvents driver.merchantOperatingCityId driver.id driver.deviceToken entityData merchantPN.fcmNotificationType

    updateReferralStats driverId driverStats transporterConfig merchOpCityId = do
      QDriverStats.updateTotalReferralCount (driverStats.totalReferralCounts + 1) driverId
      localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
      mbDailyStats <- QDailyStats.findByDriverIdAndDate driverId (utctDay localTime)
      merchantOperatingCity <- CQMOC.findById merchOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchOpCityId.getId)
      case mbDailyStats of
        Just dailyStats -> do
          Redis.withWaitOnLockRedisWithExpiry (DAP.payoutProcessingLockKey driverId.getId) 3 3 $ do
            QDailyStats.updateReferralCount (dailyStats.referralCounts + 1) driverId (utctDay localTime)
        Nothing -> do
          id <- generateGUIDText
          now <- getCurrentTime
          let dailyStatsOfDriver =
                DDS.DailyStats
                  { id = id,
                    driverId = driverId,
                    totalEarnings = 0.0,
                    numRides = 0,
                    totalDistance = 0,
                    tollCharges = 0.0,
                    bonusEarnings = 0.0,
                    merchantLocalDate = utctDay localTime,
                    currency = merchantOperatingCity.currency,
                    distanceUnit = merchantOperatingCity.distanceUnit,
                    activatedValidRides = 0,
                    referralEarnings = 0.0,
                    referralCounts = 1,
                    payoutStatus = DDS.Initialized,
                    payoutOrderId = Nothing,
                    payoutOrderStatus = Nothing,
                    createdAt = now,
                    updatedAt = now,
                    cancellationCharges = 0.0,
                    tipAmount = 0.0,
                    totalRideTime = 0,
                    merchantId = Just merchantId,
                    merchantOperatingCityId = Just merchantOperatingCity.id
                  }
          QDailyStats.create dailyStatsOfDriver
