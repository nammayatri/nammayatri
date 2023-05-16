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
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as BeamE
import Storage.Tabular.Estimate ()

create :: Estimate -> SqlDB ()
create = Esq.create

createMany :: [Estimate] -> SqlDB ()
createMany = Esq.createMany

findById :: Transactionable m => Id Estimate -> m (Maybe Estimate)
findById = Esq.findById

transformBeamEstimateToDomain :: BeamE.Estimate -> Estimate
transformBeamEstimateToDomain BeamE.EstimateT {..} = do
  Estimate
    { id = Id id,
      transactionId = transactionId,
      vehicleVariant = vehicleVariant,
      minFare = minFare,
      maxFare = maxFare,
      estimateBreakupList = estimateBreakupList,
      nightShiftRate = NightShiftRate nightShiftMultiplier nightShiftStart nightShiftEnd,
      waitingCharges = WaitingCharges waitingTimeEstimatedThreshold waitingChargePerMin waitingOrPickupCharges,
      createdAt = createdAt
    }

transformDomainEstimateToBeam :: Estimate -> BeamE.Estimate
transformDomainEstimateToBeam Estimate {..} =
  BeamE.defaultEstimate
    { BeamE.id = getId id,
      BeamE.transactionId = transactionId,
      BeamE.vehicleVariant = vehicleVariant,
      BeamE.minFare = minFare,
      BeamE.maxFare = maxFare,
      BeamE.estimateBreakupList = estimateBreakupList,
      BeamE.nightShiftMultiplier = nightShiftMultiplier $ nightShiftRate,
      BeamE.nightShiftStart = nightShiftStart $ nightShiftRate,
      BeamE.nightShiftEnd = nightShiftEnd $ nightShiftRate,
      BeamE.waitingTimeEstimatedThreshold = waitingTimeEstimatedThreshold $ waitingCharges,
      BeamE.waitingChargePerMin = waitingChargePerMin $ waitingCharges,
      BeamE.waitingOrPickupCharges = waitingOrPickupCharges $ waitingCharges,
      BeamE.createdAt = createdAt
    }
