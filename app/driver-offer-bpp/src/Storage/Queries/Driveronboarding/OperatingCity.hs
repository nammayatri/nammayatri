{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Driveronboarding.OperatingCity where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Driveronboarding.OperatingCity
import Domain.Types.Organization
import Storage.Tabular.Driveronboarding.OperatingCity

create :: OperatingCity -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id OperatingCity ->
  m (Maybe OperatingCity)
findById = Esq.findById

findByorgId ::
  Transactionable m =>
  Id Organization ->
  m (Maybe OperatingCity)
findByorgId personid = do
  findOne $ do
    vechileRegCert <- from $ table @OperatingCityT
    where_ $ vechileRegCert ^. OperatingCityOrganizationId ==. val (toKey personid)
    return vechileRegCert
