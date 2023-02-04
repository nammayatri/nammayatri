module SharedLogic.FareCalculator.Flow
  ( calculateFare,
    fareSum,
    baseFareSum,
    calculateFareParameters,
    mkBreakupList,
  )
where

import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.FareParameters
import Domain.Types.FarePolicy (FarePolicy)
import Domain.Types.Merchant (Merchant)
import EulerHS.Prelude hiding (id)
import SharedLogic.FareCalculator.Calculator
  ( baseFareSum,
    calculateFareParameters,
    fareSum,
    mkBreakupList,
  )

calculateFare ::
  (Monad m, Log m) =>
  Id Merchant ->
  FarePolicy ->
  Meters ->
  UTCTime ->
  Maybe Money ->
  m FareParameters
calculateFare merchantId farePolicy distance time driverSelectedFare = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| merchantId ||+ " and vehicle variant " +|| farePolicy.vehicleVariant ||+ ""
  let fareParams = calculateFareParameters farePolicy distance time driverSelectedFare
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams
