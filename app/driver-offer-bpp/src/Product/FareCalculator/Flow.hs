module Product.FareCalculator.Flow
  ( FareParameters (..),
    ServiceHandle (..),
    calculateFare,
    doCalculateFare,
    fareSum,
  )
where

import Beckn.Storage.Esqueleto (Transactionable)
import Beckn.Types.Id
import Domain.Types.FarePolicy (FarePolicy)
import Domain.Types.Organization (Organization)
import EulerHS.Prelude hiding (id)
import Product.FareCalculator.Calculator
  ( FareParameters (..),
    calculateFareParameters,
    fareSum,
  )
import qualified Storage.Queries.FarePolicy as FarePolicyS
import Types.Error
import Utils.Common

type MonadHandler m = (MonadThrow m, Log m)

newtype ServiceHandle m = ServiceHandle
  { getFarePolicy :: Id Organization -> m (Maybe FarePolicy)
  }

serviceHandle :: Transactionable m => ServiceHandle m
serviceHandle =
  ServiceHandle
    { getFarePolicy = \orgId -> do
        FarePolicyS.findFarePolicyByOrg orgId
    }

calculateFare ::
  (Transactionable m, MonadFlow m) =>
  Id Organization ->
  Meter ->
  UTCTime ->
  m FareParameters
calculateFare = doCalculateFare serviceHandle

doCalculateFare ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Organization ->
  Meter ->
  UTCTime ->
  m FareParameters
doCalculateFare ServiceHandle {..} orgId distance startTime = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ ""
  farePolicy <- getFarePolicy orgId >>= fromMaybeM NoFarePolicy
  let fareParams = calculateFareParameters farePolicy distance startTime
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams
