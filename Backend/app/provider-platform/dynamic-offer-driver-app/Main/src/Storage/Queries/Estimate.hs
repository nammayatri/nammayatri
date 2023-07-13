{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Estimate where

import Domain.Types.Estimate as Domain
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as BeamE

create :: L.MonadFlow m => Domain.Estimate -> m ()
create estimate = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.EstimateT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainEstimateToBeam estimate)
    Nothing -> pure ()

createMany :: L.MonadFlow m => [Estimate] -> m ()
createMany = traverse_ create

findById :: L.MonadFlow m => Id Estimate -> m (Maybe Estimate)
findById (Id estimateId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.EstimateT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamEstimateToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamE.id $ Se.Eq estimateId]
    Nothing -> pure Nothing

transformBeamEstimateToDomain :: BeamE.Estimate -> Estimate
transformBeamEstimateToDomain BeamE.EstimateT {..} = do
  Estimate
    { id = Id id,
      requestId = Id requestId,
      vehicleVariant = vehicleVariant,
      minFare = minFare,
      maxFare = maxFare,
      estimateBreakupList = estimateBreakupList,
      nightShiftInfo = NightShiftInfo <$> nightShiftCharge <*> oldNightShiftCharge <*> nightShiftStart <*> nightShiftEnd,
      waitingCharges = WaitingCharges waitingChargePerMin waitingOrPickupCharges,
      specialLocationTag = specialLocationTag,
      createdAt = createdAt
    }

transformDomainEstimateToBeam :: Estimate -> BeamE.Estimate
transformDomainEstimateToBeam Estimate {..} = do
  BeamE.EstimateT
    { id = getId id,
      requestId = getId requestId,
      vehicleVariant = vehicleVariant,
      minFare = minFare,
      maxFare = maxFare,
      estimateBreakupList = estimateBreakupList,
      nightShiftCharge = nightShiftCharge <$> nightShiftInfo,
      oldNightShiftCharge = oldNightShiftCharge <$> nightShiftInfo,
      nightShiftStart = nightShiftStart <$> nightShiftInfo,
      nightShiftEnd = nightShiftEnd <$> nightShiftInfo,
      waitingChargePerMin = waitingChargePerMin waitingCharges,
      waitingOrPickupCharges = waitingOrPickupCharges waitingCharges,
      specialLocationTag = specialLocationTag,
      createdAt = createdAt
    }
