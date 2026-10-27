{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PassengerLuggage
  ( validatePassengerLuggageFit,
    validatePassengerLuggageCounts,
    validatePassengerLuggageOnTier,
  )
where

import qualified Domain.Types.Estimate as DEstimate
import Kernel.Prelude
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.Validation
import Tools.Error

-- The most bags a group may carry above the free luggage space of the car.
maxExtraLuggageBags :: Int
maxExtraLuggageBags = 2

-- Checked only when the counts would be stored, which is when every selected estimate has the feature on.
validatePassengerLuggageFit :: MonadFlow m => Maybe Int -> Maybe Int -> Maybe Int -> [DEstimate.Estimate] -> m ()
validatePassengerLuggageFit mbPassengers mbCheckInBags mbCabinBags selectedEstimates =
  whenJust mbPassengers $ \passengers ->
    when (all (\selected -> selected.luggageFitEnabled == Just True) selectedEstimates) $
      forM_ selectedEstimates $ \selected ->
        validatePassengerLuggageOnTier passengers mbCheckInBags mbCabinBags (show selected.vehicleServiceTierType) selected.vehicleServiceTierLuggageCapacity selected.vehicleServiceTierSeatingCapacity

-- The counts need only be positive here. The upper limits come from the selected car in 'validatePassengerLuggageOnTier'.
validatePassengerLuggageCounts :: Validate (Maybe Int, Maybe Int, Maybe Int)
validatePassengerLuggageCounts (numberOfPassengers, numberOfCheckInBags, numberOfCabinBags) =
  sequenceA_
    [ validateField "numberOfPassengers" numberOfPassengers $ InMaybe $ Min @Int 1,
      validateField "numberOfCheckInBags" numberOfCheckInBags $ InMaybe $ Min @Int 0,
      validateField "numberOfCabinBags" numberOfCabinBags $ InMaybe $ Min @Int 0
    ]

-- Passengers must fit the seats. The load, in half-units, must stay within the free space plus the extra bags
-- and within the free space plus the empty seats.
-- A missing free space counts as 0. A missing seat count leaves only the extra bag limit.
-- The load is summed as Integer so that a very large count cannot wrap around and pass the check.
validatePassengerLuggageOnTier :: MonadFlow m => Int -> Maybe Int -> Maybe Int -> Text -> Maybe Int -> Maybe Int -> m ()
validatePassengerLuggageOnTier passengers mbCheckInBags mbCabinBags tierName mbFreeSpace mbSeats = do
  let loadHalfUnits = 2 * toInteger (fromMaybe 0 mbCheckInBags) + toInteger (fromMaybe 0 mbCabinBags)
      freeSpace = toInteger (fromMaybe 0 mbFreeSpace)
      extraBagLimit = 2 * (freeSpace + toInteger maxExtraLuggageBags)
      allowedHalfUnits = maybe extraBagLimit (\seats -> min extraBagLimit (2 * (freeSpace + toInteger seats - toInteger passengers))) mbSeats
  whenJust mbSeats $ \seats ->
    when (passengers > seats) $
      throwError $ PassengersExceedSeats passengers seats tierName
  when (loadHalfUnits > allowedHalfUnits) $
    throwError $ TooMuchLuggage tierName passengers allowedHalfUnits loadHalfUnits
