{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FarePolicy
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.FarePolicy
import Domain.Types.Merchant
import Domain.Types.Vehicle.Variant (Variant)
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy as BeamFP
import Storage.Tabular.FarePolicy

findAllByMerchantId ::
  Transactionable m =>
  Id Merchant ->
  m [FarePolicy]
findAllByMerchantId merchantId = do
  Esq.findAll $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
    return farePolicy

findByMerchantIdAndVariant ::
  Transactionable m =>
  Id Merchant ->
  Variant ->
  m (Maybe FarePolicy)
findByMerchantIdAndVariant merchantId variant = do
  Esq.findOne $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
        &&. farePolicy ^. FarePolicyVehicleVariant ==. val variant
    return farePolicy

findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
findById = Esq.findById

update :: FarePolicy -> SqlDB ()
update farePolicy = do
  now <- getCurrentTime
  void $
    Esq.update $ \tbl -> do
      set
        tbl
        [ FarePolicyBaseDistanceFare =. val farePolicy.baseDistanceFare,
          FarePolicyBaseDistanceMeters =. val farePolicy.baseDistanceMeters,
          FarePolicyPerExtraKmFare =. val farePolicy.perExtraKmFare,
          FarePolicyDeadKmFare =. val farePolicy.deadKmFare,
          FarePolicyDriverMinExtraFee =. val farePolicy.driverExtraFee.minFee,
          FarePolicyDriverMaxExtraFee =. val farePolicy.driverExtraFee.maxFee,
          FarePolicyNightShiftStart =. val farePolicy.nightShiftStart,
          FarePolicyNightShiftEnd =. val farePolicy.nightShiftEnd,
          FarePolicyNightShiftRate =. val farePolicy.nightShiftRate,
          FarePolicyUpdatedAt =. val now
        ]
      where_ $ tbl ^. FarePolicyTId ==. val (toKey farePolicy.id)

transformBeamFarePolicyToDomain :: BeamFP.FarePolicy -> FarePolicy
transformBeamFarePolicyToDomain BeamFP.FarePolicyT {..} = do
  FarePolicy
    { id = Id id,
      merchantId = Id merchantId,
      vehicleVariant = vehicleVariant,
      baseDistanceFare = baseDistanceFare,
      baseDistanceMeters = baseDistanceMeters,
      perExtraKmFare = perExtraKmFare,
      deadKmFare = deadKmFare,
      driverExtraFee = ExtraFee driverMinExtraFee driverMaxExtraFee,
      nightShiftRate = nightShiftRate,
      nightShiftStart = nightShiftStart,
      nightShiftEnd = nightShiftEnd,
      maxAllowedTripDistance = maxAllowedTripDistance,
      minAllowedTripDistance = minAllowedTripDistance,
      waitingChargePerMin = waitingChargePerMin,
      waitingTimeEstimatedThreshold = waitingTimeEstimatedThreshold,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainFarePolicyToBeam :: FarePolicy -> BeamFP.FarePolicy
transformDomainFarePolicyToBeam FarePolicy {..} =
  BeamFP.defaultFarePolicy
    { BeamFP.id = getId id,
      BeamFP.merchantId = getId merchantId,
      BeamFP.vehicleVariant = vehicleVariant,
      BeamFP.baseDistanceFare = baseDistanceFare,
      BeamFP.baseDistanceMeters = baseDistanceMeters,
      BeamFP.perExtraKmFare = perExtraKmFare,
      BeamFP.deadKmFare = deadKmFare,
      BeamFP.driverMinExtraFee = minFee driverExtraFee,
      BeamFP.driverMaxExtraFee = maxFee driverExtraFee,
      BeamFP.nightShiftRate = nightShiftRate,
      BeamFP.nightShiftStart = nightShiftStart,
      BeamFP.nightShiftEnd = nightShiftEnd,
      BeamFP.maxAllowedTripDistance = maxAllowedTripDistance,
      BeamFP.minAllowedTripDistance = minAllowedTripDistance,
      BeamFP.waitingChargePerMin = waitingChargePerMin,
      BeamFP.waitingTimeEstimatedThreshold = waitingTimeEstimatedThreshold,
      BeamFP.createdAt = createdAt,
      BeamFP.updatedAt = updatedAt
    }
