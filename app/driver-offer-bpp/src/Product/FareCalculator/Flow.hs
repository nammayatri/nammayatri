module Product.FareCalculator.Flow
  ( ServiceHandle (..),
    calculateFare,
    doCalculateFare,
    fareSum,
  )
where

import Beckn.Storage.Esqueleto (Transactionable)
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.FareParams
import Domain.Types.FarePolicy (FarePolicy)
import Domain.Types.Organization (Organization)
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.Prelude hiding (id)
import Product.FareCalculator.Calculator
  ( calculateFareParameters,
    fareSum,
  )
import qualified Storage.Queries.FarePolicy as FarePolicyS
import Types.Error
import Utils.Common

type MonadHandler m = (MonadThrow m, Log m)

newtype ServiceHandle m = ServiceHandle
  { getFarePolicy :: Id Organization -> Variant -> m (Maybe FarePolicy)
  }

serviceHandle :: Transactionable m => ServiceHandle m
serviceHandle =
  ServiceHandle
    { getFarePolicy = FarePolicyS.findFarePolicyByOrgAndVariant
    }

calculateFare ::
  (Transactionable m, MonadFlow m) =>
  Id Organization ->
  Variant ->
  HighPrecMeters ->
  UTCTime ->
  Maybe Amount ->
  m FareParameters
calculateFare = doCalculateFare serviceHandle

doCalculateFare ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Organization ->
  Variant ->
  HighPrecMeters ->
  UTCTime ->
  Maybe Amount ->
  m FareParameters
doCalculateFare ServiceHandle {..} orgId variant distance startTime driverSelectedFare = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ ""
  farePolicy <- getFarePolicy orgId variant >>= fromMaybeM NoFarePolicy
  let fareParams = calculateFareParameters farePolicy distance startTime driverSelectedFare
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams
