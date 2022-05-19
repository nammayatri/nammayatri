module Product.RentalFareCalculator.Flow
  ( RentalFareParameters (..),
    ServiceHandle (..),
    calculateRentalFare,
    doCalculateRentalFare,
    rentalFareSum,
    rentalFareSumWithDiscount,
  )
where

import Beckn.Types.Id
import qualified Domain.Types.RentalFarePolicy as DRentalFP
import EulerHS.Prelude hiding (id)
import Product.RentalFareCalculator.Calculator
  ( RentalFareParameters (..),
    calculateRentalFareParameters,
    rentalFareSum,
    rentalFareSumWithDiscount,
  )
import qualified Storage.Queries.RentalFarePolicy as QRentalFP
import Types.Error
import Utils.Common

type MonadHandler m = (MonadThrow m, Log m)

newtype ServiceHandle m = ServiceHandle
  { getRentalFarePolicy :: Id DRentalFP.RentalFarePolicy -> m (Maybe DRentalFP.RentalFarePolicy)
  }

serviceHandle :: EsqDBFlow m r => ServiceHandle m
serviceHandle =
  ServiceHandle
    { getRentalFarePolicy = \rentalFarePolicyId -> do
        QRentalFP.findById rentalFarePolicyId
    }

calculateRentalFare ::
  EsqDBFlow m r =>
  Id DRentalFP.RentalFarePolicy ->
  HighPrecMeters ->
  UTCTime ->
  UTCTime ->
  m RentalFareParameters
calculateRentalFare = doCalculateRentalFare serviceHandle

doCalculateRentalFare ::
  MonadHandler m =>
  ServiceHandle m ->
  Id DRentalFP.RentalFarePolicy ->
  HighPrecMeters ->
  UTCTime ->
  UTCTime ->
  m RentalFareParameters
doCalculateRentalFare ServiceHandle {..} rentalFarePolicyId distance startTime stopTime = do
  rentalFarePolicy <-
    getRentalFarePolicy rentalFarePolicyId
      >>= fromMaybeM NoRentalFarePolicy
  logTagInfo "RentalFareCalculator" $
    "Initiating rental fare calculation for organization "
      +|| rentalFarePolicy.organizationId ||+ " for "
      +|| rentalFarePolicy.vehicleVariant ||+ ""
  let fareParams = calculateRentalFareParameters rentalFarePolicy distance startTime stopTime
  logTagInfo
    "RentalFareCalculator"
    $ "Rental fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams
