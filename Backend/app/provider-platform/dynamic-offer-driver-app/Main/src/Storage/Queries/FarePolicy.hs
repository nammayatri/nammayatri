{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- module Storage.Queries.FarePolicy
--   {-# WARNING
--     "This module contains direct calls to the table. \
--   \ But most likely you need a version from CachedQueries with caching results feature."
--     #-}
-- where

-- import Domain.Types.FarePolicy
-- import Domain.Types.Merchant
-- import Domain.Types.Vehicle.Variant (Variant)
-- import qualified EulerHS.Extra.EulerDB as Extra
-- import qualified EulerHS.KVConnector.Flow as KV
-- import EulerHS.KVConnector.Types
-- import qualified EulerHS.Language as L
-- import Kernel.Prelude
-- import Kernel.Storage.Esqueleto as Esq
-- import Kernel.Types.Id
-- import Kernel.Utils.Common
-- import qualified Lib.Mesh as Mesh
-- import qualified Sequelize as Se
-- import qualified Storage.Beam.FarePolicy as BeamFP
-- import Storage.Tabular.FarePolicy

-- findAllByMerchantId ::
--   Transactionable m =>
--   Id Merchant ->
--   m [FarePolicy]
-- findAllByMerchantId merchantId = do
--   Esq.findAll $ do
--     farePolicy <- from $ table @FarePolicyT
--     where_ $
--       farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
--     return farePolicy

-- findByMerchantIdAndVariant ::
--   Transactionable m =>
--   Id Merchant ->
--   Variant ->
--   m (Maybe FarePolicy)
-- findByMerchantIdAndVariant merchantId variant = do
--   Esq.findOne $ do
--     farePolicy <- from $ table @FarePolicyT
--     where_ $
--       farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
--         &&. farePolicy ^. FarePolicyVehicleVariant ==. val variant
--     return farePolicy

-- findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
-- findById = Esq.findById

-- update :: FarePolicy -> SqlDB ()
-- update farePolicy = do
--   now <- getCurrentTime
--   void $
--     Esq.update $ \tbl -> do
--       set
--         tbl
--         [ FarePolicyBaseDistanceFare =. val farePolicy.baseDistanceFare,
--           FarePolicyBaseDistanceMeters =. val farePolicy.baseDistanceMeters,
--           FarePolicyPerExtraKmFare =. val farePolicy.perExtraKmFare,
--           FarePolicyDeadKmFare =. val farePolicy.deadKmFare,
--           FarePolicyDriverMinExtraFee =. val farePolicy.driverExtraFee.minFee,
--           FarePolicyDriverMaxExtraFee =. val farePolicy.driverExtraFee.maxFee,
--           FarePolicyNightShiftStart =. val farePolicy.nightShiftStart,
--           FarePolicyNightShiftEnd =. val farePolicy.nightShiftEnd,
--           FarePolicyNightShiftRate =. val farePolicy.nightShiftRate,
--           FarePolicyUpdatedAt =. val now
--         ]
--       where_ $ tbl ^. FarePolicyTId ==. val (toKey farePolicy.id)

-- transformBeamFarePolicyToDomain :: BeamFP.FarePolicy -> FarePolicy
-- transformBeamFarePolicyToDomain BeamFP.FarePolicyT {..} = do
--   FarePolicy
--     { id = Id id,
--       merchantId = Id merchantId,
--       vehicleVariant = vehicleVariant,
--       farePolicyType = farePolicyType,
--       driverExtraFeeBounds = ExtraFee driverMinExtraFee driverMaxExtraFee,
--       serviceCharge = serviceCharge,
--       nightShiftStart = nightShiftStart,
--       nightShiftEnd = nightShiftEnd,
--       maxAllowedTripDistance = maxAllowedTripDistance,
--       minAllowedTripDistance = minAllowedTripDistance,
--       govtCharges = govtCharges,
--       createdAt = createdAt,
--       updatedAt = updatedAt
--     }

-- transformDomainFarePolicyToBeam :: FarePolicy -> BeamFP.FarePolicy
-- transformDomainFarePolicyToBeam FarePolicy {..} =
--   BeamFP.defaultFarePolicy
--     { BeamFP.id = getId id,
--       BeamFP.merchantId = getId merchantId,
--       BeamFP.vehicleVariant = vehicleVariant,
--       BeamFP.farePolicyType = farePolicyType,
--       BeamFP.driverMinExtraFee = minFee driverExtraFee,
--       BeamFP.driverMaxExtraFee = maxFee driverExtraFee,
--       BeamFP.serviceCharge = serviceCharge,
--       BeamFP.nightShiftStart = nightShiftStart,
--       BeamFP.nightShiftEnd = nightShiftEnd,
--       BeamFP.maxAllowedTripDistance = maxAllowedTripDistance,
--       BeamFP.minAllowedTripDistance = minAllowedTripDistance,
--       BeamFP.govtCharges = govtCharges,
--       BeamFP.createdAt = createdAt,
--       BeamFP.updatedAt = updatedAt
--     }

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

import Data.List.NonEmpty
import Domain.Types.FarePolicy as Domain
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
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails as BeamFPPD
import qualified Storage.Queries.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab as QFPSlabDetSlabs
import qualified Storage.Queries.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab as QueriesFPSDS
import Storage.Queries.FullEntityBuilders (buildFullFarePolicy)
import Storage.Tabular.FarePolicy
import Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails (EntityField (FarePolicyProgressiveDetailsBaseDistance, FarePolicyProgressiveDetailsBaseFare, FarePolicyProgressiveDetailsDeadKmFare, FarePolicyProgressiveDetailsNightShiftCharge, FarePolicyProgressiveDetailsPerExtraKmFare, FarePolicyProgressiveDetailsTId), FarePolicyProgressiveDetailsT (..))
import Storage.Tabular.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab ()
import Storage.Tabular.FarePolicy.Instances
import qualified Storage.Tabular.VechileNew as VN

findAllByMerchantId ::
  Transactionable m =>
  Id Merchant ->
  m [FarePolicy]
findAllByMerchantId merchantId = buildDType $ do
  res <- Esq.findAll' $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
    return farePolicy
  catMaybes <$> mapM buildFullFarePolicy res

findByMerchantIdAndVariant ::
  Transactionable m =>
  Id Merchant ->
  Variant ->
  m (Maybe FarePolicy)
findByMerchantIdAndVariant merchantId variant = buildDType $ do
  res <- Esq.findOne' $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
        &&. farePolicy ^. FarePolicyVehicleVariant ==. val variant
    return farePolicy
  join <$> mapM buildFullFarePolicy res

findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
findById farePolicyId = buildDType $ do
  res <- Esq.findById' farePolicyId
  join <$> mapM buildFullFarePolicy res

-- findById' :: L.MonadFlow m => Id FarePolicy -> m (Maybe FarePolicy)
-- findById' (Id farePolicyId) = do
--   dbConf <- L.getOption Extra.EulerPsqlDbCfg
--   case dbConf of
--     Just dbCOnf' -> either (pure Nothing) (transformBeamFarePolicyToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamFP.id $ Se.Eq farePolicyId]
--     Nothing -> pure Nothing

update :: FarePolicy -> SqlDB ()
update farePolicy = do
  now <- getCurrentTime
  withFullEntity farePolicy $ \(FarePolicyT {..}, fpDetailsT) -> do
    void $
      Esq.update' $ \tbl -> do
        set
          tbl
          [ FarePolicyDriverMinExtraFee =. val driverMinExtraFee,
            FarePolicyDriverMaxExtraFee =. val driverMaxExtraFee,
            FarePolicyNightShiftStart =. val nightShiftStart,
            FarePolicyNightShiftEnd =. val nightShiftEnd,
            FarePolicyUpdatedAt =. val now
          ]
        where_ $ tbl ^. FarePolicyTId ==. val (toKey farePolicy.id)

    case fpDetailsT of
      ProgressiveDetailsT fpdd -> updateProgressiveDetails fpdd
      SlabsDetailsT fsdd -> updateSlabsDetails fsdd
  where
    updateProgressiveDetails FarePolicyProgressiveDetailsT {..} = do
      void $
        Esq.update' $ \tbl -> do
          set
            tbl
            [ FarePolicyProgressiveDetailsBaseFare =. val baseFare,
              FarePolicyProgressiveDetailsBaseDistance =. val baseDistance,
              FarePolicyProgressiveDetailsPerExtraKmFare =. val perExtraKmFare,
              FarePolicyProgressiveDetailsDeadKmFare =. val deadKmFare,
              FarePolicyProgressiveDetailsNightShiftCharge =. val nightShiftCharge
            ]
          where_ $ tbl ^. FarePolicyProgressiveDetailsTId ==. val (toKey farePolicy.id)
    updateSlabsDetails dets = do
      QFPSlabDetSlabs.deleteAll' farePolicy.id
      Esq.createMany' dets

transformBeamFarePolicyToDomain :: L.MonadFlow m => BeamFP.FarePolicy -> m (FarePolicy)
transformBeamFarePolicyToDomain BeamFP.FarePolicyT {..} = do
  fullFPPD <- BeamFPPD.findById' (Id id)
  let fPPD = snd $ fromJust fullFPPD
  -- fullslabs <- QueriesFPSDS.findById'' (Id id)
  -- let slabs = snd $ fromJust fullslabs
  fullslabs <- QueriesFPSDS.findAll'' (Id id)
  let slabs = snd <$> fullslabs
  pure
    FarePolicy
      { id = Id id,
        merchantId = Id merchantId,
        vehicleVariant = vehicleVariant,
        driverExtraFeeBounds = DriverExtraFeeBounds <$> driverMinExtraFee <*> driverMaxExtraFee,
        serviceCharge = serviceCharge,
        nightShiftBounds = NightShiftBounds <$> nightShiftStart <*> nightShiftEnd,
        allowedTripDistanceBounds = AllowedTripDistanceBounds <$> maxAllowedTripDistance <*> minAllowedTripDistance,
        govtCharges = govtCharges,
        farePolicyDetails = case farePolicyType of
          Progressive -> ProgressiveDetails fPPD
          Slabs -> SlabsDetails (FPSlabsDetails (fromJust $ nonEmpty slabs)),
        createdAt = createdAt,
        updatedAt = updatedAt
      }

transformDomainFarePolicyToBeam :: FarePolicy -> BeamFP.FarePolicy
transformDomainFarePolicyToBeam FarePolicy {..} =
  BeamFP.FarePolicyT
    { BeamFP.id = getId id,
      BeamFP.merchantId = getId merchantId,
      BeamFP.vehicleVariant = vehicleVariant,
      BeamFP.serviceCharge = serviceCharge,
      BeamFP.driverMinExtraFee = Domain.minFee <$> driverExtraFeeBounds,
      BeamFP.driverMaxExtraFee = Domain.maxFee <$> driverExtraFeeBounds,
      BeamFP.nightShiftStart = Domain.nightShiftStart <$> nightShiftBounds,
      BeamFP.nightShiftEnd = Domain.nightShiftEnd <$> nightShiftBounds,
      BeamFP.maxAllowedTripDistance = Domain.maxAllowedTripDistance <$> allowedTripDistanceBounds,
      BeamFP.minAllowedTripDistance = Domain.minAllowedTripDistance <$> allowedTripDistanceBounds,
      BeamFP.govtCharges = govtCharges,
      BeamFP.farePolicyType = getFarePolicyType $ FarePolicy {..},
      BeamFP.createdAt = createdAt,
      BeamFP.updatedAt = updatedAt
    }
