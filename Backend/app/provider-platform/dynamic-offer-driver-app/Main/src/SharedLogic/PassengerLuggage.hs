{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PassengerLuggage
  ( PassengerLuggage (..),
    storedPassengerLuggage,
    fitsLuggageCapacity,
    excessLuggageHalfUnits,
    findExcessLuggageHalfUnits,
    excessLuggageCharge,
    luggageFareAdjustment,
  )
where

import Domain.Types.Common (ServiceTierType)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.VehicleServiceTier
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, MonadFlow)
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST

data PassengerLuggage = PassengerLuggage
  { passengers :: Int,
    checkInBags :: Int,
    cabinBags :: Int
  }
  deriving (Show, Eq)

-- The luggage load in half-units: a check-in bag is 2 and a cabin bag is 1.
bagsLoadHalfUnits :: Int -> Int -> Int
bagsLoadHalfUnits checkInBags cabinBags = 2 * checkInBags + cabinBags

luggageLoadHalfUnits :: PassengerLuggage -> Int
luggageLoadHalfUnits group = bagsLoadHalfUnits group.checkInBags group.cabinBags

-- The counts to store, present only when a passenger count was given. The caller checks the feature flag.
storedPassengerLuggage :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe PassengerLuggage
storedPassengerLuggage mbPassengers mbCheckInBags mbCabinBags = do
  passengers <- mbPassengers
  pure PassengerLuggage {passengers = passengers, checkInBags = fromMaybe 0 mbCheckInBags, cabinBags = fromMaybe 0 mbCabinBags}

-- The most bags a group may carry above the free trunk space of the tier.
maxExtraLuggageBags :: Int
maxExtraLuggageBags = 2

-- Arguments: the vehicle trunk, the tier trunk and the tier seats. A missing tier trunk counts as 0 and a
-- missing vehicle trunk uses the tier trunk. Passengers must fit the seats. The load must fit the vehicle
-- trunk plus the empty seats and stay within the tier trunk plus the extra bags. Missing seats leave only
-- the extra bag limit.
fitsLuggageCapacity :: Maybe Int -> Maybe Int -> Maybe Int -> PassengerLuggage -> Bool
fitsLuggageCapacity mbVehicleTrunk mbTierTrunk mbSeatingCapacity group = withinSeats && luggageLoadHalfUnits group <= allowedHalfUnits
  where
    tierTrunk = fromMaybe 0 mbTierTrunk
    vehicleTrunk = fromMaybe tierTrunk mbVehicleTrunk
    withinSeats = maybe True (group.passengers <=) mbSeatingCapacity
    extraBagLimit = 2 * (tierTrunk + maxExtraLuggageBags)
    allowedHalfUnits = maybe extraBagLimit (\seats -> min extraBagLimit (2 * (vehicleTrunk + seats - group.passengers))) mbSeatingCapacity

-- Absent counts mean there is no luggage load.
storedLuggageLoadHalfUnits :: Maybe Int -> Maybe Int -> Maybe Int
storedLuggageLoadHalfUnits Nothing Nothing = Nothing
storedLuggageLoadHalfUnits mbCheckInBags mbCabinBags = Just $ bagsLoadHalfUnits (fromMaybe 0 mbCheckInBags) (fromMaybe 0 mbCabinBags)

-- The half-units above the trunk, which are charged. A missing trunk counts as 0.
-- Luggage within the trunk gives zero, not nothing, so the fare knows stored bags price the booking.
excessLuggageHalfUnits :: VehicleServiceTier -> Maybe Int -> Maybe Int -> Maybe Int
excessLuggageHalfUnits tier mbCheckInBags mbCabinBags = do
  load <- storedLuggageLoadHalfUnits mbCheckInBags mbCabinBags
  pure $ max 0 (load - 2 * fromMaybe 0 tier.luggageCapacity)

-- The excess charged at the rate of one unit, which is two half-units.
excessLuggageCharge :: Maybe HighPrecMoney -> Maybe Int -> HighPrecMoney
excessLuggageCharge mbPerLuggageCharge mbExcessHalfUnits = fromMaybe 0 $ do
  perLuggageCharge <- mbPerLuggageCharge
  excessHalfUnits <- mbExcessHalfUnits
  pure $ perLuggageCharge * fromRational (toRational excessHalfUnits / 2)

-- The change to an estimate fare when stored bags price the booking: the excess charge replaces
-- the estimate's plain luggage charge. It is zero when the stored bags do not apply.
luggageFareAdjustment :: Maybe Int -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> HighPrecMoney
luggageFareAdjustment mbExcessHalfUnits mbPerLuggageCharge mbEstimateLuggageCharge = case mbExcessHalfUnits of
  Just _ -> excessLuggageCharge mbPerLuggageCharge mbExcessHalfUnits - fromMaybe 0 mbEstimateLuggageCharge
  Nothing -> 0

-- For stages that hold only the stored counts and the tier type, such as the driver offer and end of ride.
-- An empty load has no excess for any trunk, so it needs no tier lookup.
findExcessLuggageHalfUnits ::
  (Esq.EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  ServiceTierType ->
  Maybe Int ->
  Maybe Int ->
  m (Maybe Int)
findExcessLuggageHalfUnits _ _ Nothing Nothing = pure Nothing
findExcessLuggageHalfUnits merchantOpCityId serviceTierType mbCheckInBags mbCabinBags
  | storedLuggageLoadHalfUnits mbCheckInBags mbCabinBags == Just 0 = pure (Just 0)
  | otherwise = do
    mbTier <- CQVST.findByServiceTierTypeAndCityId serviceTierType merchantOpCityId Nothing
    pure $ mbTier >>= \tier -> excessLuggageHalfUnits tier mbCheckInBags mbCabinBags
