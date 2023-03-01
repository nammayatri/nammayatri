{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.OperatingCity where

import Domain.Types.DriverOnboarding.OperatingCity
import Domain.Types.Merchant
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.DriverOnboarding.OperatingCity

create :: OperatingCity -> SqlDB m ()
create = Esq.create

findById ::
  forall m ma.
  Transactionable ma m =>
  Proxy ma ->
  Id OperatingCity ->
  m (Maybe OperatingCity)
findById _ = Esq.findById @m @ma

findByMerchantId ::
  forall m ma.
  Transactionable ma m =>
  Id Merchant ->
  Proxy ma ->
  m (Maybe OperatingCity)
findByMerchantId personid _ = do
  findOne @m @ma $ do
    vechileRegCert <- from $ table @OperatingCityT
    where_ $ vechileRegCert ^. OperatingCityMerchantId ==. val (toKey personid)
    return vechileRegCert

findEnabledCityByName ::
  forall m ma.
  Transactionable ma m =>
  Text ->
  Proxy ma ->
  m [OperatingCity]
findEnabledCityByName city _ =
  Esq.findAll @m @ma $ do
    operatingCity <- from $ table @OperatingCityT
    where_ $
      lower_ (operatingCity ^. OperatingCityCityName) ==. val city
        &&. operatingCity ^. OperatingCityEnabled
    return operatingCity

findEnabledCityByMerchantIdAndName ::
  forall m ma.
  Transactionable ma m =>
  Id Merchant ->
  Text ->
  Proxy ma ->
  m [OperatingCity]
findEnabledCityByMerchantIdAndName merchantId city _ =
  Esq.findAll @m @ma $ do
    operatingCity <- from $ table @OperatingCityT
    where_ $
      lower_ (operatingCity ^. OperatingCityCityName) ==. val city
        &&. operatingCity ^. OperatingCityMerchantId ==. val (toKey merchantId)
        &&. operatingCity ^. OperatingCityEnabled
    return operatingCity
