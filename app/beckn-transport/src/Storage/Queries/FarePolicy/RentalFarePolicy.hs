{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.RentalFarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common (Hours, Kilometers)
import Beckn.Types.Id
import Domain.Types.FarePolicy.RentalFarePolicy
import qualified Domain.Types.FarePolicy.RentalFarePolicy as Domain
import Domain.Types.Organization
import Domain.Types.Vehicle as Vehicle
import Storage.Tabular.FarePolicy.RentalFarePolicy

create ::
  Domain.RentalFarePolicy ->
  SqlDB ()
create = Esq.create

-- it's possible to find deleted fare policies only by their id.
-- other function return only not deleted fare policies
-- (RentalFarePolicyDeleted ==. val False)

findById :: Transactionable m => Id RentalFarePolicy -> m (Maybe RentalFarePolicy)
findById = Esq.findById

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
markAllAsDeleted orgId = Esq.update $ \rentalFp -> do
  set
    rentalFp
    [RentalFarePolicyDeleted =. val True]
  where_ $ rentalFp ^. RentalFarePolicyOrganizationId ==. val (toKey orgId)

findByOffer ::
  Transactionable m =>
  Id Organization ->
  Vehicle.Variant ->
  Kilometers ->
  Hours ->
  m (Maybe RentalFarePolicy)
findByOffer orgId vehicleVariant_ baseDistance baseDuration = do
  Esq.findOne $ do
    rentalFarePolicy <- from $ table @RentalFarePolicyT
    where_ $
      rentalFarePolicy ^. RentalFarePolicyOrganizationId ==. val (toKey orgId)
        &&. rentalFarePolicy ^. RentalFarePolicyVehicleVariant ==. val vehicleVariant_
        &&. rentalFarePolicy ^. RentalFarePolicyBaseDistance ==. val baseDistance.getKilometers
        &&. rentalFarePolicy ^. RentalFarePolicyBaseDuration ==. val baseDuration.getHours
    return rentalFarePolicy
