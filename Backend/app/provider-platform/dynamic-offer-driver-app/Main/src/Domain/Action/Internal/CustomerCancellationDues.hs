{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.CustomerCancellationDues where

import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.CancellationCharges as DCC
import qualified Domain.Types.DailyStats as DDS
import Domain.Types.Merchant (Merchant)
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Merchant as SMerchant
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CancellationCharges as QCC
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error

data CancellationDuesReq = CancellationDuesReq
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data CancellationDuesDetailsRes = CancellationDuesDetailsRes
  { customerCancellationDues :: HighPrecMoney,
    customerCancellationDuesWithCurrency :: PriceAPIEntity,
    disputeChancesUsed :: Int,
    canBlockCustomer :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CustomerCancellationDuesSyncReq = CustomerCancellationDuesSyncReq
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text,
    cancellationCharges :: Maybe HighPrecMoney,
    cancellationChargesWithCurrency :: Maybe PriceAPIEntity,
    disputeChancesUsed :: Maybe Int,
    paymentMadeToDriver :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

disputeCancellationDues ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Id Merchant ->
  Context.City ->
  Maybe Text ->
  CancellationDuesReq ->
  m APISuccess
disputeCancellationDues merchantId merchantCity apiKey CancellationDuesReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  numberHash <- getDbHash customerMobileNumber
  riderDetails <- QRD.findByMobileNumberHashAndMerchant numberHash merchant.id >>= fromMaybeM (RiderDetailsDoNotExist "Mobile Number" customerMobileNumber)

  when (riderDetails.cancellationDues == 0.0) $ do
    throwError $ InvalidRequest $ "Cancellation Due Amount is 0 for riderId " <> riderDetails.id.getId
  merchantOperatingCity <- CQMM.findByMerchantIdAndCity merchantId merchantCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show merchantCity)

  transporterConfig <- CTC.findByMerchantOpCityId merchantOperatingCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCity.id.getId)
  when (riderDetails.disputeChancesUsed >= transporterConfig.cancellationFeeDisputeLimit) $ do
    throwError (DisputeChancesLimitNotMet riderDetails.id.getId (show riderDetails.disputeChancesUsed) (show transporterConfig.cancellationFeeDisputeLimit))
  let disputeChancesLeft = transporterConfig.cancellationFeeDisputeLimit - riderDetails.disputeChancesUsed
      disputeAmount = min (transporterConfig.cancellationFee * fromIntegral disputeChancesLeft) (riderDetails.cancellationDues)
  disputeChances <-
    if transporterConfig.cancellationFee == 0.0
      then do
        logWarning "Unable to calculate dispute chances used"
        return 0
      else return $ round $ disputeAmount / transporterConfig.cancellationFee
  QRD.updateDisputeChancesUsedAndCancellationDues (riderDetails.disputeChancesUsed + disputeChances) (riderDetails.cancellationDues - disputeAmount) riderDetails.id
  pure Success

getCancellationDuesDetails ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Id Merchant ->
  Context.City ->
  Maybe Text ->
  CancellationDuesReq ->
  m CancellationDuesDetailsRes
getCancellationDuesDetails merchantId merchantCity apiKey CancellationDuesReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  merchantOperatingCity <- CQMM.findByMerchantIdAndCity merchant.id merchantCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchant.shortId.getShortId <> " ,city: " <> show merchantCity)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOperatingCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCity.id.getId)
  unless transporterConfig.canAddCancellationFee $
    throwError $ CityRestrictionOnCustomerCancellationDuesAddition (show merchantCity)
  numberHash <- getDbHash customerMobileNumber
  riderDetails <- QRD.findByMobileNumberHashAndMerchant numberHash merchant.id >>= fromMaybeM (RiderDetailsDoNotExist "Mobile Number" customerMobileNumber)
  let numOfChargableCancellations =
        if transporterConfig.cancellationFee == 0.0
          then 0
          else round $ riderDetails.cancellationDues / transporterConfig.cancellationFee
  return $
    CancellationDuesDetailsRes
      { customerCancellationDues = riderDetails.cancellationDues,
        customerCancellationDuesWithCurrency = PriceAPIEntity riderDetails.cancellationDues riderDetails.currency,
        disputeChancesUsed = riderDetails.disputeChancesUsed,
        canBlockCustomer = Just (numOfChargableCancellations == transporterConfig.numOfCancellationsAllowed)
      }

customerCancellationDuesSync ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Id Merchant ->
  Context.City ->
  Maybe Text ->
  CustomerCancellationDuesSyncReq ->
  m APISuccess
customerCancellationDuesSync merchantId merchantCity apiKey req = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  numberHash <- getDbHash req.customerMobileNumber
  let reqCancellationCharges = (req.cancellationChargesWithCurrency <&> (.amount)) <|> req.cancellationCharges
  when (isJust reqCancellationCharges && isJust req.disputeChancesUsed) $ do
    throwError DisputeChancesOrCancellationDuesHasToBeNull
  merchantOperatingCity <- CQMM.findByMerchantIdAndCity merchantId merchantCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show merchantCity)
  SMerchant.checkCurrencies merchantOperatingCity.currency [req.cancellationChargesWithCurrency]
  transporterConfig <- CTC.findByMerchantOpCityId merchantOperatingCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCity.id.getId)
  riderDetails <- QRD.findByMobileNumberHashAndMerchant numberHash merchant.id >>= fromMaybeM (RiderDetailsDoNotExist "Mobile Number" req.customerMobileNumber)
  case (reqCancellationCharges, req.disputeChancesUsed) of
    (Just amountPaid, Nothing) -> do
      when (amountPaid > riderDetails.cancellationDues || amountPaid < 0.0) $ do
        throwError (CustomerCancellationDuesLimitNotMet riderDetails.id.getId)

      when (req.paymentMadeToDriver) $ do
        booking <- (QBooking.findLastCancelledByRiderId riderDetails.id) >>= fromMaybeM (BookingDoesNotExist riderDetails.id.getId)
        ride <- QRide.findOneByBookingId booking.id >>= fromMaybeM (RideDoesNotExist booking.id.getId)
        id <- generateGUID
        let cancellationCharges =
              DCC.CancellationCharges
                { driverId = ride.driverId,
                  rideId = Just ride.id,
                  cancellationCharges = amountPaid,
                  currency = riderDetails.currency,
                  ..
                }
        QCC.create cancellationCharges
        localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
        mbDailyStats <- QDailyStats.findByDriverIdAndDate ride.driverId (utctDay localTime)
        case mbDailyStats of
          Just stats -> QDailyStats.updateTipAmountByDriverId (stats.cancellationCharges + amountPaid) ride.driverId (utctDay localTime)
          Nothing -> do
            logDebug $ "DailyStats not found during cancellation chanrges for driverId : " <> ride.driverId.getId
            id' <- generateGUIDText
            now <- getCurrentTime
            let dailyStatsOfDriver' =
                  DDS.DailyStats
                    { id = id',
                      driverId = ride.driverId,
                      totalEarnings = 0.0,
                      numRides = 0,
                      totalDistance = 0,
                      tollCharges = 0.0,
                      bonusEarnings = 0.0,
                      merchantLocalDate = utctDay localTime,
                      currency = ride.currency,
                      distanceUnit = ride.distanceUnit,
                      activatedValidRides = 0,
                      referralEarnings = 0.0,
                      referralCounts = 0,
                      payoutStatus = DDS.Initialized,
                      payoutOrderId = Nothing,
                      payoutOrderStatus = Nothing,
                      createdAt = now,
                      updatedAt = now,
                      cancellationCharges = amountPaid,
                      tipAmount = 0.0,
                      totalRideTime = 0,
                      numDriversOnboarded = 0,
                      numFleetsOnboarded = 0,
                      merchantId = Just merchantId,
                      merchantOperatingCityId = Just merchantOperatingCity.id,
                      onlineDuration = Nothing
                    }
            QDailyStats.create dailyStatsOfDriver'

      disputeChances <-
        if transporterConfig.cancellationFee == 0.0
          then do
            logWarning "Unable to calculate dispute chances used"
            return 0
          else return $ round $ amountPaid / transporterConfig.cancellationFee
      QRD.updateDisputeChancesUsedAndCancellationDues (max 0 (riderDetails.disputeChancesUsed - disputeChances)) (riderDetails.cancellationDues - amountPaid) riderDetails.id
    (Nothing, Just disputeChancesUsedReq) -> do
      when (disputeChancesUsedReq > transporterConfig.cancellationFeeDisputeLimit || disputeChancesUsedReq < 0) $ do
        throwError (DisputeChancesLimitNotMet riderDetails.id.getId (show disputeChancesUsedReq) (show transporterConfig.cancellationFeeDisputeLimit))
      when (riderDetails.disputeChancesUsed >= disputeChancesUsedReq) $ do
        QRD.updateDisputeChancesUsed disputeChancesUsedReq riderDetails.id
    (_, _) -> throwError DisputeChancesOrCancellationDuesHasToBeNull
  return Success
