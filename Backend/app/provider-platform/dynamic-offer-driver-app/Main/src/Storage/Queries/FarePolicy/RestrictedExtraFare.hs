{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.RestrictedExtraFare where

import qualified Domain.Types.FarePolicy.RestrictedExtraFare as Domain
import Domain.Types.Merchant
import qualified Domain.Types.Vehicle.Variant as Vehicle
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy.RestrictedExtraFare as BeamREF
import Storage.Tabular.FarePolicy.RestrictedExtraFare

create :: Domain.RestrictedExtraFare -> SqlDB ()
create = Esq.create

create' :: L.MonadFlow m => Domain.RestrictedExtraFare -> m ()
create' restrictedExtraFare = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainRestrictedExtraFareToBeam restrictedExtraFare)
    Nothing -> pure ()

findMaxExtraFareByMerchantAndVehicle :: (Transactionable m) => Id Merchant -> Vehicle.Variant -> m [Domain.RestrictedExtraFare]
findMaxExtraFareByMerchantAndVehicle merchantId vehicleVariant = do
  findAll $ do
    restrictedExtraFare <- Esq.from $ table @RestrictedExtraFareT
    where_ $
      restrictedExtraFare ^. RestrictedExtraFareMerchantId ==. val (toKey merchantId)
        &&. restrictedExtraFare ^. RestrictedExtraFareVehicleVariant ==. val vehicleVariant
    orderBy [desc (restrictedExtraFare ^. RestrictedExtraFareMinTripDistance)]
    return restrictedExtraFare

findMaxExtraFareByMerchantAndVehicle' :: L.MonadFlow m => Id Merchant -> Vehicle.Variant -> m [Domain.RestrictedExtraFare]
findMaxExtraFareByMerchantAndVehicle' (Id merchantId) vehicleVariant = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' ->
      either (pure []) (transformBeamRestrictedExtraFareToDomain <$>)
        <$> KV.findAllWithOptionsKVConnector
          dbCOnf'
          Mesh.meshConfig
          [Se.And [Se.Is BeamREF.merchantId $ Se.Eq merchantId, Se.Is BeamREF.vehicleVariant $ Se.Eq vehicleVariant]]
          (Se.Desc BeamREF.minTripDistance)
          Nothing
          Nothing
    Nothing -> pure []

findMaxExtraFareByMerchant :: (Transactionable m) => Id Merchant -> m [Domain.RestrictedExtraFare]
findMaxExtraFareByMerchant merchantId = do
  findAll $ do
    restrictedExtraFare <- Esq.from $ table @RestrictedExtraFareT
    where_ $
      restrictedExtraFare ^. RestrictedExtraFareMerchantId ==. val (toKey merchantId)
    orderBy [desc (restrictedExtraFare ^. RestrictedExtraFareMinTripDistance)]
    return restrictedExtraFare

findMaxExtraFareByMerchant' :: L.MonadFlow m => Id Merchant -> m [Domain.RestrictedExtraFare]
findMaxExtraFareByMerchant' (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' ->
      either (pure []) (transformBeamRestrictedExtraFareToDomain <$>)
        <$> KV.findAllWithOptionsKVConnector
          dbCOnf'
          Mesh.meshConfig
          [Se.Is BeamREF.merchantId $ Se.Eq merchantId]
          (Se.Desc BeamREF.minTripDistance)
          Nothing
          Nothing
    Nothing -> pure []

transformBeamRestrictedExtraFareToDomain :: BeamREF.RestrictedExtraFare -> Domain.RestrictedExtraFare
transformBeamRestrictedExtraFareToDomain BeamREF.RestrictedExtraFareT {..} = do
  Domain.RestrictedExtraFare
    { id = Id id,
      merchantId = Id merchantId,
      vehicleVariant = vehicleVariant,
      minTripDistance = minTripDistance,
      driverMaxExtraFare = driverMaxExtraFare
    }

transformDomainRestrictedExtraFareToBeam :: Domain.RestrictedExtraFare -> BeamREF.RestrictedExtraFare
transformDomainRestrictedExtraFareToBeam Domain.RestrictedExtraFare {..} =
  BeamREF.RestrictedExtraFareT
    { BeamREF.id = getId id,
      BeamREF.merchantId = getId merchantId,
      BeamREF.vehicleVariant = vehicleVariant,
      BeamREF.minTripDistance = minTripDistance,
      BeamREF.driverMaxExtraFare = driverMaxExtraFare
    }
