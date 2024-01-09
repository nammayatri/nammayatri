{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Accessor (module Reexport, module Accessor)where

import Prelude
import Data.Lens (Lens', lens)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Maybe (Maybe)
import Common.Accessor as Reexport

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

_amount :: forall a b c. Newtype a {amount :: c | b} => Lens' a c
_amount = lens (unwrap >>> _.amount) (\oldRec newVal -> wrap ((unwrap oldRec) {amount = newVal}))

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

_title :: forall a b c. Newtype a {title :: b | c } => Lens' a b
_title = lens (unwrap >>> _.title) (\oldRec newVal -> wrap ((unwrap oldRec) { title = newVal }))

_price :: forall a b c. Newtype a {price :: b | c } => Lens' a b
_price = lens (unwrap >>> _.price) (\oldRec newVal -> wrap ((unwrap oldRec) { price = newVal }))

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

_payload :: forall a b c. Newtype a { payload :: b | c } => Lens' a b
_payload = lens (unwrap >>> _.payload) (\oldRec newVal -> wrap ((unwrap oldRec) { payload = newVal }))

_view_param :: forall a b c. Newtype a { view_param :: b | c } => Lens' a b
_view_param = lens (unwrap >>> _.view_param) (\oldRec newVal -> wrap ((unwrap oldRec) { view_param = newVal }))

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