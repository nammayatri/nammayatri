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
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.OperatingCity as BeamOC
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

transformBeamOperatingCityToDomain :: BeamOC.OperatingCity -> OperatingCity
transformBeamOperatingCityToDomain BeamOC.OperatingCityT {..} = do
  OperatingCity
    { id = Id id,
      merchantId = Id merchantId,
      cityName = cityName,
      enabled = enabled,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainOperatingCityToBeam :: OperatingCity -> BeamOC.OperatingCity
transformDomainOperatingCityToBeam OperatingCity {..} =
  BeamOC.OperatingCityT
    { BeamOC.id = getId id,
      BeamOC.merchantId = getId merchantId,
      BeamOC.cityName = cityName,
      BeamOC.enabled = enabled,
      BeamOC.createdAt = createdAt,
      BeamOC.updatedAt = updatedAt
    }
