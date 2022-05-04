{-# LANGUAGE TypeApplications #-}

module Storage.Queries.RentalFarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.RentalFarePolicy
import Storage.Tabular.RentalFarePolicy

findRentalFarePoliciesByOrg ::
  Transactionable m =>
  Id Organization ->
  m [RentalFarePolicy]
findRentalFarePoliciesByOrg orgId = do
  Esq.findAll $ do
    rentalFarePolicy <- from $ table @RentalFarePolicyT
    where_ $
      rentalFarePolicy ^. RentalFarePolicyOrganizationId ==. val (toKey orgId)
    return rentalFarePolicy
