{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FareCalculator.Flow
  ( calculateFare,
    fareSum,
    baseFareSum,
    calculateFareParameters,
    mkBreakupList,
    isNightShift,
  )
where

import Domain.Types.FareParameters
import Domain.Types.FarePolicy (FarePolicy)
import Domain.Types.Merchant (Merchant)
import Domain.Types.SlabFarePolicy (SlabFarePolicy)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Error ()
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator.Calculator
  ( baseFareSum,
    calculateFareParameters,
    calculateSlabFareParameters,
    fareSum,
    isNightShift,
    mkBreakupList,
  )

calculateFare ::
  (Monad m, Log m, MonadGuid m, MonadThrow m) =>
  Id Merchant ->
  Either FarePolicy SlabFarePolicy ->
  Meters ->
  UTCTime ->
  Maybe Money ->
  m FareParameters
calculateFare merchantId policy distance time driverSelectedFare = do
  fareParams <- case policy of
    Left farePolicy -> do
      logFareCalculatorInfo farePolicy.vehicleVariant
      calculateFareParameters farePolicy distance time driverSelectedFare
    Right slabFarePolicy -> do
      logFareCalculatorInfo slabFarePolicy.vehicleVariant
      calculateSlabFareParameters slabFarePolicy distance time driverSelectedFare
  logTagInfo "FareCalculator" $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams
  where
    logFareCalculatorInfo vehicleVariant = logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| merchantId ||+ " and vehicle variant " +|| vehicleVariant ||+ ""
