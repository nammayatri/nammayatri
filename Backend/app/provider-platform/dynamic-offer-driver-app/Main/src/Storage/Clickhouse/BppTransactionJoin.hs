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
import Domain.Types.Merchant (Merchant (..))
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity (..))
import Domain.Types.Ride
import qualified Domain.Types.Ride as Ride
import Domain.Types.RiderDetails (PayoutFlagReason)
import Domain.Types.VehicleVariant
import Kernel.External.Encryption (DbHash (..))
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Time
import Storage.Queries.RideExtra

data BppTransactionJoinT f = BppTransactionJoinT
  { bookingCustomerName :: C f Text,
    bookingTripCategory :: C f Text,
    rideShortId :: C f (ShortId Ride),
    rideCreatedAt :: C f UTCTime,
    riderDetailsCancellationDues :: C f HighPrecMoney,
    riderDetailsCreatedAt :: C f UTCTime,
    riderDetailsCurrency :: C f (Maybe Currency),
    riderDetailsDisputeChancesUsed :: C f Int,
    riderDetailsFirstRideId :: C f (Maybe Text),
    riderDetailsHasTakenValidRide :: C f Bool,
    riderDetailsHasTakenValidRideAt :: C f (Maybe UTCTime),
    riderDetailsId :: C f Text,
    riderDetailsIsDeviceIdExists :: C f (Maybe Bool),
    riderDetailsIsFlagConfirmed :: C f (Maybe Bool),
    riderDetailsMerchantId :: C f Text,
    riderDetailsMobileCountryCode :: C f Text,
    riderDetailsMobileNumberEncrypted :: C f Text,
    riderDetailsMobileNumberHash :: C f Text,
    riderDetailsNightSafetyChecks :: C f Bool,
    riderDetailsOtpCode :: C f (Maybe Text),
    riderDetailsPayoutFlagReason :: C f (Maybe PayoutFlagReason),
    riderDetailsReferralCode :: C f (Maybe Text),
    riderDetailsReferredAt :: C f (Maybe UTCTime),
    riderDetailsReferredByDriver :: C f (Maybe Text),
    riderDetailsUpdatedAt :: C f UTCTime,
    rideDetailsCreatedAt :: C f (Maybe UTCTime),
    rideDetailsDefaultServiceTierName :: C f (Maybe Text),
    rideDetailsDriverCountryCode :: C f (Maybe Text),
    rideDetailsDriverName :: C f Text,
    rideDetailsDriverNumber :: C f (Maybe Text),
    rideDetailsFleetOwnerId :: C f (Maybe Text),
    rideDetailsId :: C f (Id Ride),
    rideDetailsVehicleAge :: C f (Maybe Int),
    rideDetailsVehicleClass :: C f (Maybe Text),
    rideDetailsVehicleColor :: C f (Maybe Text),
    rideDetailsVehicleModel :: C f (Maybe Text),
    rideDetailsVehicleNumber :: C f Text,
    rideDetailsVehicleVariant :: C f (Maybe VehicleVariant),
    bookingProviderId :: C f (Id Merchant),
    bookingMerchantOperatingCityId :: C f (Id MerchantOperatingCity),
    bookingEstimatedFare :: C f HighPrecMoney,
    rideFare :: C f HighPrecMoney,
    rideFareAmount :: C f HighPrecMoney,
    rideStatus :: C f RideStatus
  }
  deriving (Generic)

deriving instance Show BppTransactionJoin

bppTransactionJoinTTable :: BppTransactionJoinT (FieldModification BppTransactionJoinT)
bppTransactionJoinTTable =
  BppTransactionJoinT
    { bookingCustomerName = "booking_customer_name",
      bookingTripCategory = "booking_trip_category",
      rideShortId = "ride_short_id",
      riderDetailsCancellationDues = "rider_details_cancellation_dues",
      riderDetailsCreatedAt = "rider_details_created_at",
      riderDetailsCurrency = "rider_details_currency",
      riderDetailsDisputeChancesUsed = "rider_details_dispute_chances_used",
      riderDetailsFirstRideId = "rider_details_first_ride_id",
      riderDetailsHasTakenValidRide = "rider_details_has_taken_valid_ride",
      riderDetailsHasTakenValidRideAt = "rider_details_has_taken_valid_ride_at",
      riderDetailsId = "rider_details_id",
      riderDetailsIsDeviceIdExists = "rider_details_is_device_id_exists",
      riderDetailsIsFlagConfirmed = "rider_details_is_flag_confirmed",
      riderDetailsMerchantId = "rider_details_merchant_id",
      riderDetailsMobileCountryCode = "rider_details_mobile_country_code",
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
      rideDetailsDriverNumber = "ride_details_driver_number",
      rideDetailsFleetOwnerId = "ride_details_fleet_owner_id",
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
      rideFareAmount = "ride_fare_amount"
    }

type BppTransactionJoin = BppTransactionJoinT Identity

findAllRideItems ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
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
  m [RideItem]
findAllRideItems merchant opCity limitVal offsetVal mbBookingStatus mbRideShortId mbCustomerPhoneDBHash mbDriverPhoneDBHash now from to = do
  _ <-
    CH.findAll $
      CH.select $
        CH.limit_ limitVal $
          CH.offset_ offsetVal $
            CH.filter_
              ( \bppTransaction _ ->
                  do
                    bppTransaction.rideCreatedAt >=. from
                    CH.&&. bppTransaction.rideCreatedAt <=. to
                    CH.&&. bppTransaction.bookingProviderId CH.==. merchant.id
                    CH.&&. (bppTransaction.bookingMerchantOperatingCityId CH.==. opCity.id)
                    CH.&&. CH.whenJust_ mbRideShortId (\rsid -> bppTransaction.rideShortId CH.==. rsid)
                    CH.&&. CH.whenJust_ mbCustomerPhoneDBHash (\cpdh -> bppTransaction.riderDetailsMobileNumberHash CH.==. (Text.pack . show . unDbHash) cpdh)
                    CH.&&. CH.whenJust_ mbDriverPhoneDBHash (\dpdh -> bppTransaction.rideDetailsDriverNumberHash CH.==. (Text.pack . show . unDbHash) dpdh)
                    CH.&&. CH.whenJust_ mbBookingStatus (\bs -> mkBookingStatusVal bppTransaction.rideStatus CH.==. bs)
              )
              (CH.all_ @CH.APP_SERVICE_CLICKHOUSE bppTransactionJoinTTable)
  pure []
  where
    mkBookingStatusVal ride =
      CH.if_ (ride.status CH.==.. CH.valColumn Ride.COMPLETED) (CH.valColumn Common.COMPLETED) $
        CH.if_ (ride.status CH.==. CH.valColumn Ride.NEW) (CH.not_ (ride.createdAt CH.<=. addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now)) (CH.valColumn Common.UPCOMING) $
          CH.if_ (ride.status CH.==. CH.valColumn Ride.NEW CH.&&. (ride.createdAt CH.<=. addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now)) (CH.valColumn Common.UPCOMING_6HRS) $
            CH.if_ (ride.status CH.==. CH.valColumn Ride.INPROGRESS CH.&&. CH.not_ (ride.tripStartTime CH.<=. Just $ addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now)) (CH.valColumn Common.ONGOING) $
              CH.if_ (ride.status CH.==. CH.valColumn Ride.CANCELLED) (CH.valColumn Common.CANCELLED) (CH.valColumn Common.ONGOING_6HRS)

$(TH.mkClickhouseInstances ''BppTransactionJoinT 'SELECT_FINAL_MODIFIER)

--
-- srfd.createdAt >=. CH.DateTime from
--               CH.&&. srfd.createdAt <=. CH.DateTime to
--               CH.&&. srfd.mode CH.==. Just DI.ONLINE
--               CH.&&. CH.isNotNull srfd.fromLocGeohash
