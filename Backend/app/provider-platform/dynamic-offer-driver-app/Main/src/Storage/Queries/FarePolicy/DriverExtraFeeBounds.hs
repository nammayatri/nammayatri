{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.DriverExtraFeeBounds where

import Control.Lens ((^?), _head)
import Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as DFP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy.DriverExtraFeeBounds as BeamDEFB

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DFP.FullDriverExtraFeeBounds -> m ()
create = createWithKV

findByFarePolicyIdAndStartDistance :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> Meters -> m (Maybe DFP.FullDriverExtraFeeBounds)
findByFarePolicyIdAndStartDistance (Id farePolicyId) startDistance = findAllWithKV [Se.And [Se.Is BeamDEFB.farePolicyId $ Se.Eq farePolicyId, Se.Is BeamDEFB.startDistance $ Se.Eq startDistance]] <&> (^? _head)

update :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> Meters -> HighPrecMoney -> HighPrecMoney -> m ()
update (Id farePolicyId) startDistance minFee maxFee =
  updateWithKV
    [ Se.Set BeamDEFB.minFee $ roundToIntegral minFee,
      Se.Set BeamDEFB.maxFee $ roundToIntegral maxFee,
      Se.Set BeamDEFB.minFeeAmount $ Just minFee,
      Se.Set BeamDEFB.maxFeeAmount $ Just maxFee
    ]
    [Se.And [Se.Is BeamDEFB.farePolicyId $ Se.Eq farePolicyId, Se.Is BeamDEFB.startDistance $ Se.Eq startDistance]]

findAll' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m [DFP.FullDriverExtraFeeBounds]
findAll' farePolicyId = findAllWithOptionsKV [Se.Is BeamDEFB.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc BeamDEFB.startDistance) Nothing Nothing

deleteAll' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m ()
deleteAll' farePolicyId = deleteWithKV [Se.Is BeamDEFB.farePolicyId $ Se.Eq (getId farePolicyId)]

instance FromTType' BeamDEFB.DriverExtraFeeBounds DFP.FullDriverExtraFeeBounds where
  fromTType' BeamDEFB.DriverExtraFeeBoundsT {..} = do
    pure $
      Just
        ( KTI.Id farePolicyId,
          DFP.DriverExtraFeeBounds
            { startDistance = startDistance,
              stepFee = mkAmountWithDefault stepFeeAmount stepFee,
              defaultStepFee = mkAmountWithDefault defaultStepFeeAmount defaultStepFee,
              minFee = mkAmountWithDefault minFeeAmount minFee,
              maxFee = mkAmountWithDefault maxFeeAmount maxFee,
              distanceUnit = fromMaybe Meter distanceUnit
            }
        )

instance ToTType' BeamDEFB.DriverExtraFeeBounds DFP.FullDriverExtraFeeBounds where
  toTType' (KTI.Id farePolicyId, DFP.DriverExtraFeeBounds {..}) =
    BeamDEFB.DriverExtraFeeBoundsT
      { id = Nothing,
        farePolicyId = farePolicyId,
        startDistance = startDistance,
        minFee = roundToIntegral minFee,
        maxFee = roundToIntegral maxFee,
        stepFee = roundToIntegral stepFee,
        defaultStepFee = roundToIntegral defaultStepFee,
        minFeeAmount = Just minFee,
        maxFeeAmount = Just maxFee,
        stepFeeAmount = Just stepFee,
        defaultStepFeeAmount = Just defaultStepFee,
        distanceUnit = Just distanceUnit
      }
