{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.OperatingCity where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.DriverOnboarding.OperatingCity
import Domain.Types.Merchant
import Storage.Tabular.DriverOnboarding.OperatingCity

create :: OperatingCity -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id OperatingCity ->
  m (Maybe OperatingCity)
findById = Esq.findById

findByMerchantId ::
  Transactionable m =>
  Id Merchant ->
  m (Maybe OperatingCity)
findByMerchantId personid = do
  findOne $ do
    vechileRegCert <- from $ table @OperatingCityT
    where_ $ vechileRegCert ^. OperatingCityMerchantId ==. val (toKey personid)
    return vechileRegCert

findEnabledCityByName ::
  Transactionable m =>
  Text ->
  m [OperatingCity]
findEnabledCityByName city =
  Esq.findAll $ do
    operatingCity <- from $ table @OperatingCityT
    where_ $
      lower_ (operatingCity ^. OperatingCityCityName) ==. val city
        &&. operatingCity ^. OperatingCityEnabled
    return operatingCity

findEnabledCityByMerchantIdAndName ::
  Transactionable m =>
  Id Merchant ->
  Text ->
  m [OperatingCity]
findEnabledCityByMerchantIdAndName merchantId city =
  Esq.findAll $ do
    operatingCity <- from $ table @OperatingCityT
    where_ $
      lower_ (operatingCity ^. OperatingCityCityName) ==. val city
        &&. operatingCity ^. OperatingCityMerchantId ==. val (toKey merchantId)
        &&. operatingCity ^. OperatingCityEnabled
    return operatingCity
