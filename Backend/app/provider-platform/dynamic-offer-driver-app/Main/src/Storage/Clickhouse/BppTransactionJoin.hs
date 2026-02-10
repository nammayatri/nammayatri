{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.BppTransactionJoin where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Ride as Common
import qualified Data.Text as Text
import Domain.Types.DriverReferral
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity (..))
import Domain.Types.Person
import Domain.Types.Ride
import qualified Domain.Types.Ride as Ride
import Domain.Types.RideDetails as RideDetails
import Domain.Types.RiderDetails as RiderDetails
import Domain.Types.Trip
import Domain.Types.VehicleVariant
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.Time as Time
import Kernel.Utils.Time
import qualified Storage.Queries.RideExtra as QRE

data BppTransactionJoinT f = BppTransactionJoinT
  { bookingCustomerName :: C f (Maybe Text),
    bookingTripCategory :: C f TripCategory,
    rideShortId :: C f (ShortId Ride),
    rideCurrency :: C f (Maybe Currency),
    rideCreatedAt :: C f UTCTime,
    riderDetailsCancellationDues :: C f HighPrecMoney,
    riderDetailsCreatedAt :: C f UTCTime,
    riderDetailsCurrency :: C f Currency,
    riderDetailsDisputeChancesUsed :: C f Int,
    riderDetailsFirstRideId :: C f (Maybe Text),
    riderDetailsHasTakenValidRide :: C f Bool,
    riderDetailsHasTakenValidRideAt :: C f (Maybe UTCTime),
    riderDetailsId :: C f (Id RiderDetails),
    riderDetailsBapId :: C f (Maybe Text),
    riderDetailsIsDeviceIdExists :: C f (Maybe Bool),
    riderDetailsIsFlagConfirmed :: C f (Maybe Bool),
    riderDetailsMerchantId :: C f (Id Merchant),
    riderDetailsMobileCountryCode :: C f Text,
    riderDetailsCancellationDuesPaid :: C f HighPrecMoney,
    riderDetailsNoOfTimesCanellationDuesPaid :: C f Int,
    riderDetailsNoOfTimesWaiveOffUsed :: C f Int,
    riderDetailsWaivedOffAmount :: C f HighPrecMoney,
    riderDetailsCancelledRides :: C f Int,
    riderDetailsTotalBookings :: C f Int,
    riderDetailsCompletedRides :: C f Int,
    riderDetailsValidCancellations :: C f Int,
    riderDetailsCancellationDueRides :: C f Int,
    riderDetailsMobileNumberEncrypted :: C f Text,
    riderDetailsMobileNumberHash :: C f Text,
    riderDetailsNightSafetyChecks :: C f Bool,
    riderDetailsOtpCode :: C f (Maybe Text),
    riderDetailsPayoutFlagReason :: C f (Maybe PayoutFlagReason),
    riderDetailsReferralCode :: C f (Maybe (Id DriverReferral)),
    riderDetailsReferredAt :: C f (Maybe UTCTime),
    riderDetailsReferredByDriver :: C f (Maybe (Id Person)),
    riderDetailsUpdatedAt :: C f UTCTime,
    rideDetailsCreatedAt :: C f (Maybe UTCTime),
    rideDetailsDefaultServiceTierName :: C f (Maybe Text),
    rideDetailsDriverCountryCode :: C f (Maybe Text),
    rideDetailsDriverName :: C f Text,
    rideDetailsDriverNumberHash :: C f (Maybe Text),
    rideDetailsDriverNumberEncrypted :: C f (Maybe Text),
    rideDetailsFleetOwnerId :: C f (Maybe Text),
    rideDetailsRcId :: C f (Maybe Text),
    rideDetailsId :: C f (Id Ride),
    rideDetailsVehicleAge :: C f (Maybe Time.Months),
    rideDetailsVehicleClass :: C f (Maybe Text),
    rideDetailsVehicleColor :: C f (Maybe Text),
    rideDetailsVehicleModel :: C f (Maybe Text),
    rideDetailsVehicleNumber :: C f Text,
    rideDetailsVehicleVariant :: C f (Maybe VehicleVariant),
    bookingProviderId :: C f (Id Merchant),
    bookingMerchantOperatingCityId :: C f (Id MerchantOperatingCity),
    bookingEstimatedFare :: C f (Maybe HighPrecMoney),
    rideFare :: C f (Maybe HighPrecMoney),
    rideFareAmount :: C f HighPrecMoney,
    rideFleetOwnerId :: C f (Maybe Text),
    rideStatus :: C f RideStatus,
    rideTripStartTime :: C f (Maybe UTCTime)
  }
  deriving (Generic)

deriving instance Show BppTransactionJoin

bppTransactionJoinTTable :: BppTransactionJoinT (FieldModification BppTransactionJoinT)
bppTransactionJoinTTable =
  BppTransactionJoinT
    { bookingCustomerName = "booking_customer_name",
      bookingTripCategory = "booking_trip_category",
      rideShortId = "ride_short_id",
      rideCurrency = "ride_currency",
      riderDetailsCancellationDues = "rider_details_cancellation_dues",
      riderDetailsCreatedAt = "rider_details_created_at",
      riderDetailsCurrency = "rider_details_currency",
      riderDetailsDisputeChancesUsed = "rider_details_dispute_chances_used",
      riderDetailsFirstRideId = "rider_details_first_ride_id",
      riderDetailsHasTakenValidRide = "rider_details_has_taken_valid_ride",
      riderDetailsHasTakenValidRideAt = "rider_details_has_taken_valid_ride_at",
      riderDetailsId = "rider_details_id",
      riderDetailsBapId = "rider_details_bap_id",
      riderDetailsIsDeviceIdExists = "rider_details_is_device_id_exists",
      riderDetailsIsFlagConfirmed = "rider_details_is_flag_confirmed",
      riderDetailsMerchantId = "rider_details_merchant_id",
      riderDetailsMobileCountryCode = "rider_details_mobile_country_code",
      riderDetailsCancellationDuesPaid = "rider_details_cancellation_dues_paid",
      riderDetailsNoOfTimesCanellationDuesPaid = "rider_details_no_of_times_canellation_dues_paid",
      riderDetailsNoOfTimesWaiveOffUsed = "rider_details_no_of_times_waive_off_used",
      riderDetailsWaivedOffAmount = "rider_details_waived_off_amount",
      riderDetailsCancelledRides = "rider_details_cancelled_rides",
      riderDetailsTotalBookings = "rider_details_total_bookings",
      riderDetailsCompletedRides = "rider_details_completed_rides",
      riderDetailsValidCancellations = "rider_details_valid_cancellations",
      riderDetailsCancellationDueRides = "rider_details_cancellation_due_rides",
      riderDetailsMobileNumberEncrypted = "rider_details_mobile_number_encrypted",
      riderDetailsMobileNumberHash = "rider_details_mobile_number_hash",
      riderDetailsNightSafetyChecks = "rider_details_night_safety_checks",
      riderDetailsOtpCode = "rider_details_otp_code",
      riderDetailsPayoutFlagReason = "rider_details_payout_flag_reason",
      riderDetailsReferralCode = "rider_details_referral_code",
      riderDetailsReferredAt = "rider_details_referred_at",
      riderDetailsReferredByDriver = "rider_details_referred_by_driver",
      riderDetailsUpdatedAt = "rider_details_updated_at",
      rideDetailsCreatedAt = "ride_details_created_at",
      rideDetailsDefaultServiceTierName = "ride_details_default_service_tier_name",
      rideDetailsDriverCountryCode = "ride_details_driver_country_code",
      rideDetailsDriverName = "ride_details_driver_name",
      rideDetailsDriverNumberHash = "ride_details_driver_number_hash",
      rideDetailsDriverNumberEncrypted = "ride_details_driver_number_encrypted",
      rideDetailsFleetOwnerId = "ride_details_fleet_owner_id",
      rideDetailsRcId = "ride_details_rc_id",
      rideDetailsId = "ride_details_id",
      rideDetailsVehicleAge = "ride_details_vehicle_age",
      rideDetailsVehicleClass = "ride_details_vehicle_class",
      rideDetailsVehicleColor = "ride_details_vehicle_color",
      rideDetailsVehicleModel = "ride_details_vehicle_model",
      rideDetailsVehicleNumber = "ride_details_vehicle_number",
      rideDetailsVehicleVariant = "ride_details_vehicle_variant",
      rideCreatedAt = "ride_created_at",
      bookingProviderId = "booking_provider_id",
      bookingMerchantOperatingCityId = "booking_merchant_operating_city_id",
      bookingEstimatedFare = "booking_estimated_fare",
      rideFare = "ride_fare",
      rideFareAmount = "ride_fare_amount",
      rideFleetOwnerId = "ride_fleet_owner_id",
      rideTripStartTime = "ride_trip_start_time",
      rideStatus = "ride_status"
    }

type BppTransactionJoin = BppTransactionJoinT Identity

instance ClickhouseValue RideStatus

instance ClickhouseValue Common.BookingStatus

instance ClickhouseValue Time.Months

instance ClickhouseValue TripCategory

instance CH.ClickhouseValue PayoutFlagReason

$(TH.mkClickhouseInstances ''BppTransactionJoinT 'SELECT_FINAL_MODIFIER)

findAllRideItems ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Maybe Bool ->
  Merchant ->
  MerchantOperatingCity ->
  Int ->
  Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Ride) ->
  Maybe DbHash ->
  Maybe DbHash ->
  UTCTime ->
  UTCTime ->
  UTCTime ->
  Maybe Text ->
  Maybe Text ->
  m [QRE.RideItem]
findAllRideItems _isDashboardRequest merchant opCity limitVal offsetVal mbBookingStatus mbRideShortId mbCustomerPhoneDBHash mbDriverPhoneDBHash now from to mbVehicleNo mbFleetOwnerId = do
  bppTransaction <-
    CH.findAll $
      CH.select $
        CH.limit_ limitVal $
          CH.offset_ offsetVal $
            CH.filter_
              ( \bppTransaction ->
                  do
                    bppTransaction.rideCreatedAt >=. from
                    CH.&&. bppTransaction.rideCreatedAt <=. to
                    CH.&&. bppTransaction.bookingProviderId CH.==. merchant.id
                    CH.&&. (bppTransaction.bookingMerchantOperatingCityId CH.==. opCity.id)
                    CH.&&. CH.whenJust_ mbRideShortId (\rsid -> bppTransaction.rideShortId CH.==. rsid)
                    CH.&&. CH.whenJust_ mbCustomerPhoneDBHash (\cpdh -> bppTransaction.riderDetailsMobileNumberHash CH.==. (Text.pack . show . unDbHash) cpdh)
                    CH.&&. CH.whenJust_ mbDriverPhoneDBHash (\dpdh -> bppTransaction.rideDetailsDriverNumberHash CH.==. Just ((Text.pack . show . unDbHash) dpdh))
                    CH.&&. CH.whenJust_ mbVehicleNo (\vehicleNo -> bppTransaction.rideDetailsVehicleNumber CH.==. vehicleNo)
                    CH.&&. CH.whenJust_ mbFleetOwnerId (\foid -> bppTransaction.rideFleetOwnerId CH.==. Just foid)
                    CH.&&. CH.whenJust_ mbBookingStatus (`mkBookingStatusCond` bppTransaction)
              )
              (CH.all_ @CH.APP_SERVICE_CLICKHOUSE bppTransactionJoinTTable)
  return $ fmap mkRideItem bppTransaction
  where
    mkBookingStatus ride
      | ride.rideStatus == Ride.COMPLETED = Common.COMPLETED
      | ride.rideStatus == Ride.NEW && (ride.rideCreatedAt) > subtract6Hrs = Common.UPCOMING
      | ride.rideStatus == Ride.NEW && ride.rideCreatedAt <= subtract6Hrs = Common.UPCOMING_6HRS
      | ride.rideStatus == Ride.INPROGRESS && ride.rideTripStartTime > Just subtract6Hrs = Common.ONGOING
      | ride.rideStatus == Ride.CANCELLED = Common.CANCELLED
      | otherwise = Common.ONGOING_6HRS

    subtract6Hrs = addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now

    mkBookingStatusCond Common.COMPLETED ride = ride.rideStatus CH.==. Ride.COMPLETED
    mkBookingStatusCond Common.UPCOMING ride = ride.rideStatus CH.==. Ride.NEW CH.&&. ride.rideCreatedAt CH.>. subtract6Hrs
    mkBookingStatusCond Common.UPCOMING_6HRS ride = ride.rideStatus CH.==. Ride.NEW CH.&&. ride.rideCreatedAt CH.<=. subtract6Hrs
    mkBookingStatusCond Common.ONGOING ride = ride.rideStatus CH.==. Ride.INPROGRESS CH.&&. ride.rideTripStartTime CH.>. Just subtract6Hrs
    mkBookingStatusCond Common.ONGOING_6HRS ride = ride.rideStatus CH.==. Ride.INPROGRESS CH.&&. ride.rideTripStartTime CH.<=. Just subtract6Hrs
    mkBookingStatusCond Common.CANCELLED ride = ride.rideStatus CH.==. Ride.CANCELLED

    mkRideItem bppTxn@BppTransactionJoinT {..} =
      QRE.RideItem
        { rideDetails =
            RideDetails.RideDetails
              { RideDetails.createdAt = bppTxn.rideDetailsCreatedAt,
                RideDetails.defaultServiceTierName = bppTxn.rideDetailsDefaultServiceTierName,
                RideDetails.driverCountryCode = bppTxn.rideDetailsDriverCountryCode,
                RideDetails.driverName = bppTxn.rideDetailsDriverName,
                RideDetails.driverNumber = EncryptedHashed <$> (Encrypted <$> bppTxn.rideDetailsDriverNumberEncrypted) <*> (DbHash <$> (encodeUtf8 <$> bppTxn.rideDetailsDriverNumberHash)),
                RideDetails.fleetOwnerId = bppTxn.rideDetailsFleetOwnerId,
                RideDetails.rcId = bppTxn.rideDetailsRcId,
                RideDetails.id = bppTxn.rideDetailsId,
                RideDetails.vehicleAge = bppTxn.rideDetailsVehicleAge,
                RideDetails.vehicleClass = bppTxn.rideDetailsVehicleClass,
                RideDetails.vehicleColor = bppTxn.rideDetailsVehicleColor,
                RideDetails.vehicleModel = bppTxn.rideDetailsVehicleModel,
                RideDetails.vehicleNumber = bppTxn.rideDetailsVehicleNumber,
                RideDetails.vehicleVariant = bppTxn.rideDetailsVehicleVariant,
                RideDetails.merchantId = Just merchant.id,
                RideDetails.merchantOperatingCityId = Just opCity.id
              },
          riderDetails =
            RiderDetails.RiderDetails
              { RiderDetails.cancellationDues = bppTxn.riderDetailsCancellationDues,
                RiderDetails.createdAt = bppTxn.riderDetailsCreatedAt,
                RiderDetails.currency = bppTxn.riderDetailsCurrency,
                RiderDetails.disputeChancesUsed = bppTxn.riderDetailsDisputeChancesUsed,
                RiderDetails.firstRideId = bppTxn.riderDetailsFirstRideId,
                RiderDetails.hasTakenValidRide = bppTxn.riderDetailsHasTakenValidRide,
                RiderDetails.hasTakenValidRideAt = bppTxn.riderDetailsHasTakenValidRideAt,
                RiderDetails.id = bppTxn.riderDetailsId,
                RiderDetails.isDeviceIdExists = bppTxn.riderDetailsIsDeviceIdExists,
                RiderDetails.isFlagConfirmed = bppTxn.riderDetailsIsFlagConfirmed,
                RiderDetails.merchantId = bppTxn.riderDetailsMerchantId,
                RiderDetails.mobileCountryCode = bppTxn.riderDetailsMobileCountryCode,
                RiderDetails.mobileNumber = EncryptedHashed (Encrypted bppTxn.riderDetailsMobileNumberEncrypted) (DbHash (encodeUtf8 bppTxn.riderDetailsMobileNumberHash)),
                RiderDetails.nightSafetyChecks = bppTxn.riderDetailsNightSafetyChecks,
                RiderDetails.otpCode = bppTxn.riderDetailsOtpCode,
                RiderDetails.payoutFlagReason = bppTxn.riderDetailsPayoutFlagReason,
                RiderDetails.referralCode = bppTxn.riderDetailsReferralCode,
                RiderDetails.referredAt = bppTxn.riderDetailsReferredAt,
                RiderDetails.referredByDriver = bppTxn.riderDetailsReferredByDriver,
                RiderDetails.updatedAt = bppTxn.riderDetailsUpdatedAt,
                RiderDetails.merchantOperatingCityId = Just opCity.id,
                RiderDetails.cancellationDuesPaid = bppTxn.riderDetailsCancellationDuesPaid,
                RiderDetails.noOfTimesCanellationDuesPaid = bppTxn.riderDetailsNoOfTimesCanellationDuesPaid,
                RiderDetails.noOfTimesWaiveOffUsed = bppTxn.riderDetailsNoOfTimesWaiveOffUsed,
                RiderDetails.waivedOffAmount = bppTxn.riderDetailsWaivedOffAmount,
                RiderDetails.bapId = bppTxn.riderDetailsBapId,
                RiderDetails.cancelledRides = bppTxn.riderDetailsCancelledRides,
                RiderDetails.totalBookings = bppTxn.riderDetailsTotalBookings,
                RiderDetails.completedRides = bppTxn.riderDetailsCompletedRides,
                RiderDetails.validCancellations = bppTxn.riderDetailsValidCancellations,
                RiderDetails.cancellationDueRides = bppTxn.riderDetailsCancellationDueRides
              },
          customerName = bppTxn.bookingCustomerName,
          fareDiff = mkPrice bppTxn.rideCurrency <$> (bppTxn.rideFare - bppTxn.bookingEstimatedFare),
          bookingStatus = mkBookingStatus bppTxn,
          tripCategory = bppTxn.bookingTripCategory,
          ..
        }

findAllRideItemsV2 ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Merchant ->
  MerchantOperatingCity ->
  Int ->
  Int ->
  Maybe Ride.RideStatus ->
  Maybe (ShortId Ride) ->
  Maybe DbHash ->
  Maybe DbHash ->
  UTCTime ->
  UTCTime ->
  m [QRE.RideItemV2]
findAllRideItemsV2 merchant opCity limitVal offsetVal mbRideStatus mbRideShortId mbCustomerPhoneDBHash mbDriverPhoneDBHash from to = do
  bppTransaction <-
    CH.findAll $
      CH.select $
        CH.limit_ limitVal $
          CH.offset_ offsetVal $
            CH.filter_
              ( \bppTransaction ->
                  do
                    bppTransaction.rideCreatedAt >=. from
                    CH.&&. bppTransaction.rideCreatedAt <=. to
                    CH.&&. bppTransaction.bookingProviderId CH.==. merchant.id
                    CH.&&. (bppTransaction.bookingMerchantOperatingCityId CH.==. opCity.id)
                    CH.&&. CH.whenJust_ mbRideShortId (\rsid -> bppTransaction.rideShortId CH.==. rsid)
                    CH.&&. CH.whenJust_ mbCustomerPhoneDBHash (\cpdh -> bppTransaction.riderDetailsMobileNumberHash CH.==. (Text.pack . show . unDbHash) cpdh)
                    CH.&&. CH.whenJust_ mbDriverPhoneDBHash (\dpdh -> bppTransaction.rideDetailsDriverNumberHash CH.==. Just ((Text.pack . show . unDbHash) dpdh))
                    CH.&&. CH.whenJust_ mbRideStatus (\status -> bppTransaction.rideStatus CH.==. status)
              )
              (CH.all_ @CH.APP_SERVICE_CLICKHOUSE bppTransactionJoinTTable)
  return $ fmap mkRideItemV2 bppTransaction
  where
    mkRideItemV2 bppTxn =
      QRE.RideItemV2
        { rideShortId = bppTxn.rideShortId,
          rideCreatedAt = bppTxn.rideCreatedAt,
          rideId = bppTxn.rideDetailsId,
          driverName = bppTxn.rideDetailsDriverName,
          driverPhoneNo = EncryptedHashed <$> (Encrypted <$> bppTxn.rideDetailsDriverNumberEncrypted) <*> (DbHash <$> (encodeUtf8 <$> bppTxn.rideDetailsDriverNumberHash)),
          rideStatus = bppTxn.rideStatus
        }
