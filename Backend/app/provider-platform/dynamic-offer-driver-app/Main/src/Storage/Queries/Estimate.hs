{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Estimate where

import Data.Coerce (coerce)
import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Estimate as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as BeamE

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.Estimate -> m ()
create = createWithKV

createMany :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Estimate] -> m ()
createMany = traverse_ create

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Estimate -> m (Maybe Estimate)
findById (Id estimateId) = findOneWithKV [Se.Is BeamE.id $ Se.Eq estimateId]

instance FromTType' BeamE.Estimate Estimate where
  fromTType' BeamE.EstimateT {..} = do
    pure $
      Just
        Estimate
          { id = Id id,
            requestId = Id requestId,
            vehicleVariant = vehicleVariant,
            minFare = minFare,
            maxFare = maxFare,
            estimateBreakupList = coerce @[EstimateBreakupD 'Unsafe] @[EstimateBreakup] estimateBreakupList,
            nightShiftInfo = NightShiftInfo <$> nightShiftCharge <*> oldNightShiftCharge <*> nightShiftStart' <*> nightShiftEnd',
            waitingCharges = WaitingCharges waitingChargePerMin waitingOrPickupCharges,
            specialLocationTag = specialLocationTag,
            createdAt = createdAt
          }
    where
      nightShiftStart' = case nightShiftStart of
        (Just (BeamE.TimeOfDayText nightShiftStart'')) -> Just nightShiftStart''
        Nothing -> Nothing
      nightShiftEnd' = case nightShiftEnd of
        (Just (BeamE.TimeOfDayText nightShiftEnd'')) -> Just nightShiftEnd''
        Nothing -> Nothing

instance ToTType' BeamE.Estimate Estimate where
  toTType' Estimate {..} = do
    BeamE.EstimateT
      { id = getId id,
        requestId = getId requestId,
        vehicleVariant = vehicleVariant,
        minFare = minFare,
        maxFare = maxFare,
        estimateBreakupList = coerce @[EstimateBreakup] @[EstimateBreakupD 'Unsafe] estimateBreakupList,
        nightShiftCharge = nightShiftCharge <$> nightShiftInfo,
        oldNightShiftCharge = oldNightShiftCharge <$> nightShiftInfo,
        nightShiftStart = BeamE.TimeOfDayText . nightShiftStart <$> nightShiftInfo,
        nightShiftEnd = BeamE.TimeOfDayText . nightShiftEnd <$> nightShiftInfo,
        waitingChargePerMin = waitingChargePerMin waitingCharges,
        waitingOrPickupCharges = waitingOrPickupCharges waitingCharges,
        specialLocationTag = specialLocationTag,
        createdAt = createdAt
      }
