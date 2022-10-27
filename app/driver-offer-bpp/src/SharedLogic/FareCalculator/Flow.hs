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
import Domain.Types.FareParams
import Domain.Types.FarePolicy (FarePolicy)
import Domain.Types.Organization (Organization)
import EulerHS.Prelude hiding (id)
import SharedLogic.FareCalculator.Calculator
  ( baseFareSum,
    calculateFareParameters,
    fareSum,
    mkBreakupList,
  )

calculateFare ::
  (Monad m, Log m) =>
  Id Organization ->
  FarePolicy ->
  Meters ->
  UTCTime ->
  Maybe Money ->
  m FareParameters
calculateFare orgId farePolicy distance time driverSelectedFare = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ " and vehicle variant " +|| farePolicy.vehicleVariant ||+ ""
  let fareParams = calculateFareParameters farePolicy distance time driverSelectedFare
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams
