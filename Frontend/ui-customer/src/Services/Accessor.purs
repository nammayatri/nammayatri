{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Accessor (
  module Accessor,
  module Reexport
) where

import Engineering.Helpers.Accessor as Reexport

import Prelude
import Data.Lens (Lens', lens)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Maybe (Maybe)

_formattedAddress :: forall a b c. Newtype a { formattedAddress :: b | c} => Lens' a b
_formattedAddress = lens (unwrap >>> _.formattedAddress) (\oldRec newVal -> wrap ((unwrap oldRec) {formattedAddress = newVal}))

_distance_meters :: forall a b c. Newtype a { distance :: b | c} => Lens' a b
_distance_meters = lens (unwrap >>> _.distance) (\oldRec newVal -> wrap ((unwrap oldRec) {distance = newVal}))

_description :: forall a b c. Newtype a {description :: c | b} => Lens' a c
_description = lens (unwrap >>> _.description) (\oldRec newVal -> wrap ((unwrap oldRec) {description = newVal}))

_place_id :: forall a b c. Newtype a {placeId :: c | b} => Lens' a c
_place_id = lens (unwrap >>> _.placeId) (\oldRec newVal -> wrap ((unwrap oldRec) {placeId = newVal}))

_distance :: forall a b c. Newtype a {distance :: c | b} => Lens' a c
_distance = lens (unwrap >>> _.distance) (\oldRec newVal -> wrap ((unwrap oldRec) {distance = newVal}))

_result :: forall a b c. Newtype a {result :: c | b} => Lens' a c
_result = lens (unwrap >>> _.result) (\oldRec newVal -> wrap ((unwrap oldRec) {result = newVal}))

_location :: forall a b c. Newtype a {location :: c | b} => Lens' a c
_location = lens (unwrap >>> _.location) (\oldRec newVal -> wrap ((unwrap oldRec) {location = newVal}))

_vehicleVariant :: forall a b c. Newtype a {vehicleVariant :: c | b} => Lens' a c
_vehicleVariant = lens (unwrap >>> _.vehicleVariant) (\oldRec newVal -> wrap ((unwrap oldRec) {vehicleVariant = newVal}))

_estimatedFare :: forall a b c. Newtype a {estimatedTotalFare :: c | b} => Lens' a c
_estimatedFare = lens (unwrap >>> _.estimatedTotalFare) (\oldRec newVal -> wrap ((unwrap oldRec) {estimatedTotalFare = newVal}))

_estimateId :: forall a b c. Newtype a {id :: c | b} => Lens' a c
_estimateId = lens (unwrap >>> _.id) (\oldRec newVal -> wrap ((unwrap oldRec) {id = newVal}))

_contents :: forall a b c. Newtype a {contents :: c | b} => Lens' a c
_contents = lens (unwrap >>> _.contents) (\oldRec newVal -> wrap ((unwrap oldRec) {contents = newVal}))

_toLocation :: forall a b c. Newtype a {toLocation :: c | b} => Lens' a c
_toLocation = lens (unwrap >>> _.toLocation) (\oldRec newVal -> wrap ((unwrap oldRec) {toLocation = newVal}))

_amount :: forall a b c. Newtype a {amount :: c | b} => Lens' a c -- Deprecated
_amount = lens (unwrap >>> _.amount) (\oldRec newVal -> wrap ((unwrap oldRec) {amount = newVal}))

_amountWithCurrency :: forall a b c. Newtype a {amountWithCurrency :: c | b} => Lens' a c
_amountWithCurrency = lens (unwrap >>> _.amountWithCurrency) (\oldRec newVal -> wrap ((unwrap oldRec) {amountWithCurrency = newVal}))

_driverRatings :: forall a b c. Newtype a {driverRatings :: c | b} => Lens' a c
_driverRatings = lens (unwrap >>> _.driverRatings) (\oldRec newVal -> wrap ((unwrap oldRec) {driverRatings = newVal}))

_driverName :: forall a b c. Newtype a {driverName :: c | b} => Lens' a c
_driverName = lens (unwrap >>> _.driverName) (\oldRec newVal -> wrap ((unwrap oldRec) {driverName = newVal}))

_status :: forall a b c. Newtype a { status :: b | c } => Lens' a b
_status = lens (unwrap >>> _.status) (\oldRec newVal -> wrap ((unwrap oldRec) { status = newVal }))

_lat :: forall a b c. Newtype a { lat :: b | c } => Lens' a b
_lat = lens (unwrap >>> _.lat) (\oldRec newVal -> wrap ((unwrap oldRec) { lat = newVal }))

_lon :: forall a b c. Newtype a { lon :: b | c } => Lens' a b
_lon = lens (unwrap >>> _.lon) (\oldRec newVal -> wrap ((unwrap oldRec) { lon = newVal }))

_vehicleNumber :: forall a b c. Newtype a { vehicleNumber :: b | c } => Lens' a b
_vehicleNumber = lens (unwrap >>> _.vehicleNumber) (\oldRec newVal -> wrap ((unwrap oldRec) { vehicleNumber = newVal }))


_id :: forall a b c. Newtype a { id :: b | c } => Lens' a b
_id = lens (unwrap >>> _.id) (\oldRec newVal -> wrap ((unwrap oldRec) { id = newVal }))

_list :: forall a b c. Newtype a { list :: b | c } => Lens' a b
_list = lens (unwrap >>> _.list) (\oldRec newVal -> wrap ((unwrap oldRec) { list = newVal }))

_computedPrice :: forall a b c. Newtype a {  computedPrice :: b | c } => Lens' a b
_computedPrice = lens (unwrap >>> _.computedPrice) (\oldRec newVal -> wrap ((unwrap oldRec) { computedPrice = newVal }))

_shortRideId :: forall a b c. Newtype a { shortRideId :: b | c } => Lens' a b
_shortRideId = lens (unwrap >>> _.shortRideId) (\oldRec newVal -> wrap ((unwrap oldRec) { shortRideId = newVal }))

_rideRating :: forall a b c. Newtype a {rideRating :: b | c } => Lens' a b
_rideRating = lens (unwrap >>> _.rideRating) (\oldRec newVal -> wrap ((unwrap oldRec) { rideRating = newVal }))

_results :: forall a b c. Newtype a {results :: b | c } => Lens' a b
_results = lens (unwrap >>> _.results) (\oldRec newVal -> wrap ((unwrap oldRec) { results = newVal }))

_geometry :: forall a b c. Newtype a {geometry :: b | c } => Lens' a b
_geometry = lens (unwrap >>> _.geometry) (\oldRec newVal -> wrap ((unwrap oldRec) { geometry = newVal }))

_estimateFareBreakup :: forall a b c. Newtype a {estimateFareBreakup :: b | c } => Lens' a b
_estimateFareBreakup = lens (unwrap >>> _.estimateFareBreakup) (\oldRec newVal -> wrap ((unwrap oldRec) { estimateFareBreakup = newVal }))

_totalFareRange :: forall a b c. Newtype a {totalFareRange :: b | c } => Lens' a b
_totalFareRange = lens (unwrap >>> _.totalFareRange) (\oldRec newVal -> wrap ((unwrap oldRec) { totalFareRange = newVal }))

_maxFare :: forall a b c. Newtype a {maxFare :: b | c } => Lens' a b
_maxFare = lens (unwrap >>> _.maxFare) (\oldRec newVal -> wrap ((unwrap oldRec) { maxFare = newVal }))

_minFare :: forall a b c. Newtype a {minFare :: b | c } => Lens' a b
_minFare = lens (unwrap >>> _.minFare) (\oldRec newVal -> wrap ((unwrap oldRec) { minFare = newVal }))

_nightShiftRate :: forall a b c. Newtype a {nightShiftRate :: b | c } => Lens' a b
_nightShiftRate = lens (unwrap >>> _.nightShiftRate) (\oldRec newVal -> wrap ((unwrap oldRec) { nightShiftRate = newVal }))

_nightShiftEnd :: forall a b c. Newtype a {nightShiftEnd :: b | c } => Lens' a b
_nightShiftEnd = lens (unwrap >>> _.nightShiftEnd) (\oldRec newVal -> wrap ((unwrap oldRec) { nightShiftEnd = newVal }))

_nightShiftMultiplier :: forall a b c. Newtype a {nightShiftMultiplier :: b | c } => Lens' a b
_nightShiftMultiplier = lens (unwrap >>> _.nightShiftMultiplier) (\oldRec newVal -> wrap ((unwrap oldRec) { nightShiftMultiplier = newVal }))

_nightShiftStart :: forall a b c. Newtype a {nightShiftStart :: b | c } => Lens' a b
_nightShiftStart = lens (unwrap >>> _.nightShiftStart) (\oldRec newVal -> wrap ((unwrap oldRec) { nightShiftStart = newVal }))

_estimatedDistance :: forall a b c. Newtype a {estimatedDistance :: b | c } => Lens' a b
_estimatedDistance = lens (unwrap >>> _.estimatedDistance) (\oldRec newVal -> wrap ((unwrap oldRec) { estimatedDistance = newVal }))

_chargeableRideDistance :: forall a b c. Newtype a { chargeableRideDistance :: b | c } => Lens' a b
_chargeableRideDistance = lens (unwrap >>> _.chargeableRideDistance) (\oldRec newVal -> wrap ((unwrap oldRec) { chargeableRideDistance = newVal }))

_selectedQuotes :: forall a b c. Newtype a { selectedQuotes :: b | c } => Lens' a b
_selectedQuotes = lens (unwrap >>> _.selectedQuotes) (\oldRec newVal -> wrap ((unwrap oldRec) { selectedQuotes = newVal }))

_otpCode :: forall a b c. Newtype a { otpCode :: b | c } => Lens' a b
_otpCode = lens (unwrap >>> _.otpCode) (\oldRec newVal -> wrap ((unwrap oldRec) { otpCode = newVal }))

_signatureAuthData :: forall a b c. Newtype a { signatureAuthData :: b | c } => Lens' a b
_signatureAuthData = lens (unwrap >>> _.signatureAuthData) (\oldRec newVal -> wrap ((unwrap oldRec) { signatureAuthData = newVal }))

_fareProductType :: forall a b c. Newtype a { fareProductType :: b | c } => Lens' a b
_fareProductType = lens (unwrap >>> _.fareProductType) (\oldRec newVal -> wrap ((unwrap oldRec) { fareProductType = newVal }))

_specialLocationTag :: forall a b c. Newtype a { specialLocationTag :: b | c } => Lens' a b
_specialLocationTag = lens (unwrap >>> _.specialLocationTag) (\oldRec newVal -> wrap ((unwrap oldRec) { specialLocationTag = newVal }))

_search_type :: forall a b c. Newtype a { search_type :: b | c } => Lens' a b
_search_type = lens (unwrap >>> _.search_type) (\oldRec newVal -> wrap ((unwrap oldRec) { search_type = newVal }))

_source :: forall a b c. Newtype a { source :: b | c } => Lens' a b
_source = lens (unwrap >>> _.source) (\oldRec newVal -> wrap ((unwrap oldRec) { source = newVal }))

_destination :: forall a b c. Newtype a { destination :: b | c } => Lens' a b
_destination = lens (unwrap >>> _.destination) (\oldRec newVal -> wrap ((unwrap oldRec) { destination = newVal }))

_deviceToken :: forall a b c. Newtype a { deviceToken :: b | c } => Lens' a b
_deviceToken = lens (unwrap >>> _.deviceToken) (\oldRec newVal -> wrap ((unwrap oldRec) { deviceToken = newVal }))

_ticketShortId :: forall a b c. Newtype a { ticketShortId :: b | c } => Lens' a b
_ticketShortId = lens (unwrap >>> _.ticketShortId) (\oldRec newVal -> wrap ((unwrap oldRec) { ticketShortId = newVal }))

_ticketPlaceId :: forall a b c. Newtype a { ticketPlaceId :: b | c } => Lens' a b
_ticketPlaceId = lens (unwrap >>> _.ticketPlaceId) (\oldRec newVal -> wrap ((unwrap oldRec) { ticketPlaceId = newVal }))

_ticketPlaceName :: forall a b c. Newtype a { ticketPlaceName :: b | c } => Lens' a b
_ticketPlaceName = lens (unwrap >>> _.ticketPlaceName) (\oldRec newVal -> wrap ((unwrap oldRec) { ticketPlaceName = newVal }))

_personId :: forall a b c. Newtype a { personId :: b | c } => Lens' a b
_personId = lens (unwrap >>> _.personId) (\oldRec newVal -> wrap ((unwrap oldRec) { personId = newVal }))

_visitDate :: forall a b c. Newtype a { visitDate :: b | c } => Lens' a b
_visitDate = lens (unwrap >>> _.visitDate) (\oldRec newVal -> wrap ((unwrap oldRec) { visitDate = newVal }))

_services :: forall a b c. Newtype a { services :: b | c } => Lens' a b
_services = lens (unwrap >>> _.services) (\oldRec newVal -> wrap ((unwrap oldRec) { services = newVal }))

_show_splash :: forall a b c. Newtype a { show_splash :: b | c } => Lens' a b
_show_splash = lens (unwrap >>> _.show_splash) (\oldRec newVal -> wrap ((unwrap oldRec) { show_splash = newVal }))

_firstName :: forall a b c. Newtype a { firstName :: b | c } => Lens' a b
_firstName = lens (unwrap >>> _.firstName) (\oldRec newVal -> wrap ((unwrap oldRec) { firstName = newVal }))

_middleName :: forall a b c. Newtype a { middleName :: b | c } => Lens' a b
_middleName = lens (unwrap >>> _.middleName) (\oldRec newVal -> wrap ((unwrap oldRec) { middleName = newVal }))

_lastName :: forall a b c. Newtype a { lastName :: b | c } => Lens' a b
_lastName = lens (unwrap >>> _.lastName) (\oldRec newVal -> wrap ((unwrap oldRec) { lastName = newVal }))
_authData :: forall a b c. Newtype a { authData :: b | c } => Lens' a b
_authData = lens (unwrap >>> _.authData) (\oldRec newVal -> wrap ((unwrap oldRec) { authData = newVal }))

_maskedMobileNumber :: forall a b c. Newtype a { maskedMobileNumber :: b | c } => Lens' a b
_maskedMobileNumber = lens (unwrap >>> _.maskedMobileNumber) (\oldRec newVal -> wrap ((unwrap oldRec) { maskedMobileNumber = newVal }))

_isBlocked :: forall a b c. Newtype a { isBlocked :: b | c } => Lens' a b
_isBlocked = lens (unwrap >>> _.isBlocked) (\oldRec newVal -> wrap ((unwrap oldRec) { isBlocked = newVal }))

_major :: forall a b c. Newtype a { major :: b | c } => Lens' a b
_major = lens (unwrap >>> _.major) (\oldRec newVal -> wrap ((unwrap oldRec) { major = newVal }))

_minor :: forall a b c. Newtype a { minor :: b | c } => Lens' a b
_minor = lens (unwrap >>> _.minor) (\oldRec newVal -> wrap ((unwrap oldRec) { minor = newVal }))

_maintenance :: forall a b c. Newtype a { maintenance :: b | c } => Lens' a b
_maintenance = lens (unwrap >>> _.maintenance) (\oldRec newVal -> wrap ((unwrap oldRec) { maintenance = newVal }))

_deeplinkOptions :: forall a b c. Newtype a { deeplinkOptions :: b | c } => Lens' a b
_deeplinkOptions = lens (unwrap >>> _.deeplinkOptions) (\oldRec newVal -> wrap ((unwrap oldRec) { deeplinkOptions = newVal }))

_maskedDeviceToken :: forall a b c. Newtype a { maskedDeviceToken :: b | c } => Lens' a b
_maskedDeviceToken = lens (unwrap >>> _.maskedDeviceToken) (\oldRec newVal -> wrap ((unwrap oldRec) { maskedDeviceToken = newVal }))

_email :: forall a b c. Newtype a { email :: b | c } => Lens' a b
_email = lens (unwrap >>> _.email) (\oldRec newVal -> wrap ((unwrap oldRec) { email = newVal }))

_hasTakenRide :: forall a b c. Newtype a { hasTakenRide :: b | c } => Lens' a b
_hasTakenRide = lens (unwrap >>> _.hasTakenRide) (\oldRec newVal -> wrap ((unwrap oldRec) { hasTakenRide = newVal }))

_referralCode :: forall a b c. Newtype a { referralCode :: b | c } => Lens' a b
_referralCode = lens (unwrap >>> _.referralCode) (\oldRec newVal -> wrap ((unwrap oldRec) { referralCode = newVal }))

_language :: forall a b c. Newtype a { language :: b | c } => Lens' a b
_language = lens (unwrap >>> _.language) (\oldRec newVal -> wrap ((unwrap oldRec) { language = newVal }))

_gender :: forall a b c. Newtype a { gender :: b | c } => Lens' a b
_gender = lens (unwrap >>> _.gender) (\oldRec newVal -> wrap ((unwrap oldRec) { gender = newVal }))

_bundleVersion :: forall a b c. Newtype a { bundleVersion :: b | c } => Lens' a b
_bundleVersion = lens (unwrap >>> _.bundleVersion) (\oldRec newVal -> wrap ((unwrap oldRec) { bundleVersion = newVal }))

_clientVersion :: forall a b c. Newtype a { clientVersion :: b | c } => Lens' a b
_clientVersion = lens (unwrap >>> _.clientVersion) (\oldRec newVal -> wrap ((unwrap oldRec) { clientVersion = newVal }))

_disability :: forall a b c. Newtype a { disability :: b | c } => Lens' a b
_disability = lens (unwrap >>> _.disability) (\oldRec newVal -> wrap ((unwrap oldRec) { disability = newVal }))

_hasDisability :: forall a b c. Newtype a { hasDisability :: b | c } => Lens' a b
_hasDisability = lens (unwrap >>> _.hasDisability) (\oldRec newVal -> wrap ((unwrap oldRec) { hasDisability = newVal }))

_currentStatus :: forall a b c. Newtype a { currentStatus :: b | c } => Lens' a b
_currentStatus = lens (unwrap >>> _.currentStatus) (\oldRec newVal -> wrap ((unwrap oldRec) { currentStatus = newVal }))

_fareBreakup :: forall a b c. Newtype a { fareBreakup :: b | c } => Lens' a b
_fareBreakup = lens (unwrap >>> _.fareBreakup) (\oldRec newVal -> wrap ((unwrap oldRec) { fareBreakup = newVal }))

_paymentMethod :: forall a b c. Newtype a { payment_method :: b | c } => Lens' a b
_paymentMethod = lens (unwrap >>> _.payment_method) (\oldRec newVal -> wrap ((unwrap oldRec) { payment_method = newVal }))


_name :: forall a b c. Newtype a { name :: b | c } => Lens' a b
_name = lens (unwrap >>> _.name) (\oldRec newVal -> wrap ((unwrap oldRec) { name = newVal }))

_hasCompletedSafetySetup :: forall a b c. Newtype a { hasCompletedSafetySetup :: b | c } => Lens' a b
_hasCompletedSafetySetup = lens (unwrap >>> _.hasCompletedSafetySetup) (\oldRec newVal -> wrap ((unwrap oldRec) { hasCompletedSafetySetup = newVal }))

_hasCompletedMockSafetyDrill :: forall a b c. Newtype a { hasCompletedMockSafetyDrill :: b | c } => Lens' a b
_hasCompletedMockSafetyDrill = lens (unwrap >>> _.hasCompletedMockSafetyDrill) (\oldRec newVal -> wrap ((unwrap oldRec) { hasCompletedMockSafetyDrill = newVal }))

_followsRide :: forall a b c. Newtype a { followsRide :: b | c } => Lens' a b
_followsRide = lens (unwrap >>> _.followsRide) (\oldRec newVal -> wrap ((unwrap oldRec) { followsRide = newVal }))

_rideList :: forall a b c. Newtype a { _ideList :: b | c } => Lens' a b
_rideList = lens (unwrap >>> _._ideList) (\oldRec newVal -> wrap ((unwrap oldRec) { _ideList = newVal }))

_categoryId :: forall a b c. Newtype a { categoryId :: b | c } => Lens' a b
_categoryId = lens (unwrap >>> _.categoryId) (\oldRec newVal -> wrap ((unwrap oldRec) { categoryId = newVal }))

_cancellationRate :: forall a b c. Newtype a { cancellationRate :: b | c } => Lens' a b
_cancellationRate = lens (unwrap >>> _.cancellationRate) (\oldRec newVal -> wrap ((unwrap oldRec) { cancellationRate = newVal }))

_availableSeats :: forall a b c. Newtype a { availableSeats :: b | c } => Lens' a b
_availableSeats = lens (unwrap >>> _.availableSeats) (\oldRec newVal -> wrap ((unwrap oldRec) { availableSeats = newVal }))

_allowedSeats :: forall a b c. Newtype a { allowedSeats :: b | c } => Lens' a b
_allowedSeats = lens (unwrap >>> _.allowedSeats) (\oldRec newVal -> wrap ((unwrap oldRec) { allowedSeats = newVal }))

_bookedSeats :: forall a b c. Newtype a { bookedSeats :: b | c } => Lens' a b
_bookedSeats = lens (unwrap >>> _.bookedSeats) (\oldRec newVal -> wrap ((unwrap oldRec) { bookedSeats = newVal }))

_isClosed :: forall a b c. Newtype a { isClosed :: b | c } => Lens' a b
_isClosed = lens (unwrap >>> _.isClosed) (\oldRec newVal -> wrap ((unwrap oldRec) { isClosed = newVal }))

_peopleCategories :: forall a b c. Newtype a { peopleCategories :: b | c } => Lens' a b
_peopleCategories = lens (unwrap >>> _.peopleCategories) (\oldRec newVal -> wrap ((unwrap oldRec) { peopleCategories = newVal }))

_createdAt :: forall a b c. Newtype a { createdAt :: b | c } => Lens' a b
_createdAt = lens (unwrap >>> _.createdAt) (\oldRec newVal -> wrap ((unwrap oldRec) { createdAt = newVal }))

_isSafetyCenterDisabled :: forall a b c. Newtype a { isSafetyCenterDisabled :: b | c } => Lens' a b
_isSafetyCenterDisabled = lens (unwrap >>> _.isSafetyCenterDisabled) (\oldRec newVal -> wrap ((unwrap oldRec) { isSafetyCenterDisabled = newVal }))

_rideEndTime :: forall a b c. Newtype a { rideEndTime :: b | c } => Lens' a b
_rideEndTime = lens (unwrap >>> _.rideEndTime) (\oldRec newVal -> wrap ((unwrap oldRec) { rideEndTime = newVal }))

_customerReferralCode :: forall a b c. Newtype a { customerReferralCode :: b | c } => Lens' a b
_customerReferralCode = lens (unwrap >>> _.customerReferralCode) (\oldRec newVal -> wrap ((unwrap oldRec) { customerReferralCode = newVal }))

_serviceTierName :: forall a b c. Newtype a { serviceTierName :: b | c } => Lens' a b
_serviceTierName = lens (unwrap >>> _.serviceTierName) (\oldRec newVal -> wrap ((unwrap oldRec) { serviceTierName = newVal }))

_hasTakenValidCabRide :: forall a b c. Newtype a { hasTakenValidCabRide :: b | c } => Lens' a b
_hasTakenValidCabRide = lens (unwrap >>> _.hasTakenValidCabRide) (\oldRec newVal -> wrap ((unwrap oldRec) { hasTakenValidCabRide = newVal }))

_hasTakenValidAutoRide :: forall a b c. Newtype a { hasTakenValidAutoRide :: b | c } => Lens' a b
_hasTakenValidAutoRide = lens (unwrap >>> _.hasTakenValidAutoRide) (\oldRec newVal -> wrap ((unwrap oldRec) { hasTakenValidAutoRide = newVal }))

_hasTakenValidBikeRide :: forall a b c. Newtype a { hasTakenValidBikeRide :: b | c } => Lens' a b
_hasTakenValidBikeRide = lens (unwrap >>> _.hasTakenValidBikeRide) (\oldRec newVal -> wrap ((unwrap oldRec) { hasTakenValidBikeRide = newVal }))

_isValueAddNP :: forall a b c. Newtype a { isValueAddNP :: b | c } => Lens' a b
_isValueAddNP = lens (unwrap >>> _.isValueAddNP) (\oldRec newVal -> wrap ((unwrap oldRec) { isValueAddNP = newVal }))

_stopLocation :: forall a b c. Newtype a { stopLocation :: b | c } => Lens' a b
_stopLocation = lens (unwrap >>> _.stopLocation) (\oldRec newVal -> wrap ((unwrap oldRec) { stopLocation = newVal }))

_deviceId :: forall a b c. Newtype a { deviceId :: b | c } => Lens' a b
_deviceId = lens (unwrap >>> _.deviceId) (\oldRec newVal -> wrap ((unwrap oldRec) { deviceId = newVal }))

_androidId :: forall a b c. Newtype a { androidId :: b | c } => Lens' a b
_androidId = lens (unwrap >>> _.androidId) (\oldRec newVal -> wrap ((unwrap oldRec) { androidId = newVal }))


_chatMessageData :: forall a b c. Newtype a { chatMessageData :: b | c } => Lens' a b
_chatMessageData = lens (unwrap >>> _.chatMessageData) (\oldRec newVal -> wrap ((unwrap oldRec) { chatMessageData = newVal }))

_appToken :: forall a b c. Newtype a { appToken :: b | c } => Lens' a b
_appToken = lens (unwrap >>> _.appToken) (\oldRec newVal -> wrap ((unwrap oldRec) { appToken = newVal }))

_currency :: forall a b c. Newtype a {currency :: c | b} => Lens' a c
_currency = lens (unwrap >>> _.currency) (\oldRec newVal -> wrap ((unwrap oldRec) {currency = newVal}))

_quoteDetails :: forall a b c. Newtype a { quoteDetails :: b | c } => Lens' a b
_quoteDetails = lens (unwrap >>> _.quoteDetails) (\oldRec newVal -> wrap ((unwrap oldRec) { quoteDetails = newVal }))

_baseFare :: forall a b c. Newtype a { baseFare :: b | c } => Lens' a b
_baseFare = lens (unwrap >>> _.baseFare) (\oldRec newVal -> wrap ((unwrap oldRec) { baseFare = newVal }))

_nightShiftCharge :: forall a b c. Newtype a { nightShiftCharge :: b | c } => Lens' a b
_nightShiftCharge = lens (unwrap >>> _.nightShiftCharge) (\oldRec newVal -> wrap ((unwrap oldRec) { nightShiftCharge = newVal }))

_isScheduled :: forall a b c. Newtype a { isScheduled :: b | c } => Lens' a b
_isScheduled = lens (unwrap >>> _.isScheduled) (\oldRec newVal -> wrap ((unwrap oldRec) { isScheduled = newVal }))

_types :: forall a b c. Newtype a { types :: b | c } => Lens' a b
_types = lens (unwrap >>> _.types) (\oldRec newVal -> wrap ((unwrap oldRec) { types = newVal }))

_senderDetails :: forall a b c. Newtype a { senderDetails :: b | c } => Lens' a b
_senderDetails = lens (unwrap >>> _.senderDetails) (\oldRec newVal -> wrap ((unwrap oldRec) { senderDetails = newVal }))

_receiverDetails :: forall a b c. Newtype a { receiverDetails :: b | c } => Lens' a b
_receiverDetails = lens (unwrap >>> _.receiverDetails) (\oldRec newVal -> wrap ((unwrap oldRec) { receiverDetails = newVal }))

_initiatedAs :: forall a b c. Newtype a { initiatedAs :: b | c } => Lens' a b
_initiatedAs = lens (unwrap >>> _.initiatedAs) (\oldRec newVal -> wrap ((unwrap oldRec) { initiatedAs = newVal }))

_phoneNumber :: forall a b c. Newtype a { phoneNumber :: b | c } => Lens' a b
_phoneNumber = lens (unwrap >>> _.phoneNumber) (\oldRec newVal -> wrap ((unwrap oldRec) { phoneNumber = newVal }))

_address :: forall a b c. Newtype a { address :: b | c } => Lens' a b
_address = lens (unwrap >>> _.address) (\oldRec newVal -> wrap ((unwrap oldRec) { address = newVal }))

_instruction :: forall a b c. Newtype a { instruction :: b | c } => Lens' a b
_instruction = lens (unwrap >>> _.instruction) (\oldRec newVal -> wrap ((unwrap oldRec) { instruction = newVal }))

_extras :: forall a b c. Newtype a { extras :: b | c } => Lens' a b
_extras = lens (unwrap >>> _.extras) (\oldRec newVal -> wrap ((unwrap oldRec) { extras = newVal }))

_start :: forall a b c. Newtype a { start :: b | c } => Lens' a b
_start = lens (unwrap >>> _.start) (\oldRec newVal -> wrap ((unwrap oldRec) { start = newVal }))

_end :: forall a b c. Newtype a { end :: b | c } => Lens' a b
_end = lens (unwrap >>> _.end) (\oldRec newVal -> wrap ((unwrap oldRec) { end = newVal }))

_requestorPartyRoles :: forall a b c. Newtype a { requestorPartyRoles :: b | c } => Lens' a b
_requestorPartyRoles = lens (unwrap >>> _.requestorPartyRoles) (\oldRec newVal -> wrap ((unwrap oldRec) { requestorPartyRoles = newVal }))

_vehicleType :: forall a b c. Newtype a { vehicleType :: b | c } => Lens' a b
_vehicleType = lens (unwrap >>> _.vehicleType) (\oldRec newVal -> wrap ((unwrap oldRec) { vehicleType = newVal }))

_refundDetails :: forall a b c. Newtype a { refundDetails :: b | c } => Lens' a b
_refundDetails = lens (unwrap >>> _.refundDetails) (\oldRec newVal -> wrap ((unwrap oldRec) { refundDetails = newVal }))

_parcelQuantity :: forall a b c. Newtype a { parcelQuantity :: b | c } => Lens' a b
_parcelQuantity = lens (unwrap >>> _.parcelQuantity) (\oldRec newVal -> wrap ((unwrap oldRec) { parcelQuantity = newVal }))

_parcelType :: forall a b c. Newtype a { parcelType :: b | c } => Lens' a b
_parcelType = lens (unwrap >>> _.parcelType) (\oldRec newVal -> wrap ((unwrap oldRec) { parcelType = newVal }))

_tag :: forall a b c. Newtype a { tag :: b | c } => Lens' a b
_tag = lens (unwrap >>> _.tag) (\oldRec newVal -> wrap ((unwrap oldRec) { tag = newVal }))

_driverArrivalTime :: forall a b c. Newtype a { driverArrivalTime :: b | c } => Lens' a b
_driverArrivalTime = lens (unwrap >>> _.driverArrivalTime) (\oldRec newVal -> wrap ((unwrap oldRec) { driverArrivalTime = newVal }))

_destinationReachedAt :: forall a b c. Newtype a { destinationReachedAt :: b | c } => Lens' a b
_destinationReachedAt = lens (unwrap >>> _.destinationReachedAt) (\oldRec newVal -> wrap ((unwrap oldRec) { destinationReachedAt = newVal }))

_stopPoint :: forall a b c. Newtype a { stopPoint :: b | c } => Lens' a b
_stopPoint = lens (unwrap >>> _.stopPoint) (\oldRec newVal -> wrap ((unwrap oldRec) { stopPoint = newVal }))

_sequenceNum :: forall a b c. Newtype a { sequenceNum :: b | c } => Lens' a b
_sequenceNum = lens (unwrap >>> _.sequenceNum) (\oldRec newVal -> wrap ((unwrap oldRec) { sequenceNum = newVal }))

_code :: forall a b c. Newtype a { code :: b | c } => Lens' a b
_code = lens (unwrap >>> _.code) (\oldRec newVal -> wrap ((unwrap oldRec) { code = newVal }))

_stopCode :: forall a b c. Newtype a { stopCode :: b | c } => Lens' a b
_stopCode = lens (unwrap >>> _.stopCode) (\oldRec newVal -> wrap ((unwrap oldRec) { stopCode = newVal }))

_routeCode :: forall a b c. Newtype a { routeCode :: b | c } => Lens' a b
_routeCode = lens (unwrap >>> _.routeCode) (\oldRec newVal -> wrap ((unwrap oldRec) { routeCode = newVal }))
