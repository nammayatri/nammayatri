{-# LANGUAGE TypeApplications #-}

module Storage.Queries.RentalFarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.RentalFarePolicy
import Domain.Types.Vehicle as Vehicle
import Storage.Tabular.RentalFarePolicy

findRentalFarePolicyByOrgAndVehicleVariant ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  m (Maybe RentalFarePolicy)
findRentalFarePolicyByOrgAndVehicleVariant orgId vehicleVariant_ = do
  Esq.findOne $ do
    rentalFarePolicy <- from $ table @RentalFarePolicyT
    where_ $
      rentalFarePolicy ^. RentalFarePolicyOrganizationId ==. val (toKey orgId)
        &&. rentalFarePolicy ^. RentalFarePolicyVehicleVariant ==. val vehicleVariant_
    return rentalFarePolicy
