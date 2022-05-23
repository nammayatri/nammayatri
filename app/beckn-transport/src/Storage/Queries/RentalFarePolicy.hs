{-# LANGUAGE TypeApplications #-}

module Storage.Queries.RentalFarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.RentalFarePolicy
import qualified Domain.Types.RentalFarePolicy as Domain
import Domain.Types.Vehicle as Vehicle
import Storage.Tabular.RentalFarePolicy

create ::
  Domain.RentalFarePolicy ->
  SqlDB ()
create = Esq.create'

-- it's possible to find deleted fare policies only by their id.
-- other function return only not deleted fare policies
-- (RentalFarePolicyDeleted ==. val False)

findById ::
  Transactionable m =>
  Id Domain.RentalFarePolicy ->
  m (Maybe RentalFarePolicy)
findById rentalFpId = do
  Esq.findOne $ do
    rentalFarePolicy <- from $ table @RentalFarePolicyT
    where_ $
      rentalFarePolicy ^. RentalFarePolicyId ==. val rentalFpId.getId
    return rentalFarePolicy

findRentalFarePoliciesByOrg ::
  Transactionable m =>
  Id Organization ->
  m [RentalFarePolicy]
findRentalFarePoliciesByOrg orgId = do
  Esq.findAll $ do
    rentalFarePolicy <- from $ table @RentalFarePolicyT
    where_ $
      rentalFarePolicy ^. RentalFarePolicyOrganizationId ==. val (toKey orgId)
        &&. rentalFarePolicy ^. RentalFarePolicyDeleted ==. val False
    return rentalFarePolicy

markAllAsDeleted ::
  Id Organization ->
  SqlDB ()
markAllAsDeleted orgId = Esq.update' $ \rentalFp -> do
  set
    rentalFp
    [RentalFarePolicyDeleted =. val True]
  where_ $ rentalFp ^. RentalFarePolicyOrganizationId ==. val (toKey orgId)

findRentalFarePolicyForQuote ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  Amount ->
  m (Maybe RentalFarePolicy)
findRentalFarePolicyForQuote orgId vehicleVariant_ baseFare = do
  Esq.findOne $ do
    rentalFarePolicy <- from $ table @RentalFarePolicyT
    where_ $
      rentalFarePolicy ^. RentalFarePolicyOrganizationId ==. val (toKey orgId)
        &&. rentalFarePolicy ^. RentalFarePolicyVehicleVariant ==. val vehicleVariant_
        &&. rentalFarePolicy ^. RentalFarePolicyBaseFare ==. val baseFare
    return rentalFarePolicy
