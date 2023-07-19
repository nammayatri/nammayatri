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

import Domain.Types.Estimate as Domain
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findOneWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as BeamE

create :: (L.MonadFlow m, Log m) => Domain.Estimate -> m ()
create = createWithKV

createMany :: (L.MonadFlow m, Log m) => [Estimate] -> m ()
createMany = traverse_ create

findById :: (L.MonadFlow m, Log m) => Id Estimate -> m (Maybe Estimate)
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
            estimateBreakupList = estimateBreakupList,
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
        estimateBreakupList = estimateBreakupList,
        nightShiftCharge = nightShiftCharge <$> nightShiftInfo,
        oldNightShiftCharge = oldNightShiftCharge <$> nightShiftInfo,
        nightShiftStart = BeamE.TimeOfDayText . nightShiftStart <$> nightShiftInfo,
        nightShiftEnd = BeamE.TimeOfDayText . nightShiftEnd <$> nightShiftInfo,
        waitingChargePerMin = waitingChargePerMin waitingCharges,
        waitingOrPickupCharges = waitingOrPickupCharges waitingCharges,
        specialLocationTag = specialLocationTag,
        createdAt = createdAt
      }
