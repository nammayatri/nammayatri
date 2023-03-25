{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Accessor where

import Prelude
import Data.Lens (Lens', lens)
import Data.Newtype (class Newtype, unwrap, wrap)

_formattedAddress :: forall a b c. Newtype a { formattedAddress :: b | c} => Lens' a b
_formattedAddress = lens (unwrap >>> _.formattedAddress) (\oldRec newVal -> wrap ((unwrap oldRec) {formattedAddress = newVal}))

_description :: forall a b c. Newtype a {description :: c | b} => Lens' a c
_description = lens (unwrap >>> _.description) (\oldRec newVal -> wrap ((unwrap oldRec) {description = newVal}))

_place_id :: forall a b c. Newtype a {placeId :: c | b} => Lens' a c
_place_id = lens (unwrap >>> _.placeId) (\oldRec newVal -> wrap ((unwrap oldRec) {placeId = newVal}))

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