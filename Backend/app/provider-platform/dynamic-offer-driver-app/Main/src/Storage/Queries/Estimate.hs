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
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import Kernel.Prelude
-- import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as BeamE
import Storage.Tabular.Estimate ()

-- create :: Estimate -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => Domain.Estimate -> m ()
create estimate = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainEstimateToBeam estimate)
    Nothing -> pure ()

createMany :: L.MonadFlow m => [Estimate] -> m ()
createMany est = void $ traverse create est

-- createMany :: [Estimate] -> SqlDB ()
-- createMany = Esq.createMany

-- findById :: Transactionable m => Id Estimate -> m (Maybe Estimate)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id Estimate -> m (Maybe Estimate)
findById (Id estimateId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamEstimateToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamE.id $ Se.Eq estimateId]
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
      -- nightShiftRate = NightShiftRate nightShiftMultiplier nightShiftStart nightShiftEnd,
      nightShiftInfo = NightShiftInfo <$> nightShiftCharge <*> oldNightShiftCharge <*> nightShiftStart <*> nightShiftEnd,
      waitingCharges = WaitingCharges waitingChargePerMin waitingOrPickupCharges,
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
      createdAt = createdAt
    }
