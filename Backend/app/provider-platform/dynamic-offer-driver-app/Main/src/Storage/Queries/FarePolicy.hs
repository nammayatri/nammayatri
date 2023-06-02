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
-- import Domain.Types.Common
import Domain.Types.FarePolicy as Domain
import Domain.Types.Merchant
import Domain.Types.Vehicle.Variant (Variant)
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import Kernel.Prelude
-- import Kernel.Storage.Esqueleto ()
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy as BeamFP
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as BeamFPPD
import qualified Storage.Beam.FarePolicy.FarePolicySlabDetails.FarePolicySlabDetailsSlab as BeamFPSS
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails as QueriesFPPD
import qualified Storage.Queries.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab as QFPSlabDetSlabs
import qualified Storage.Queries.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab as QueriesFPSDS
-- import Storage.Queries.FullEntityBuilders (buildFullFarePolicy)
-- import Storage.Tabular.FarePolicy
-- import Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails (EntityField (FarePolicyProgressiveDetailsBaseDistance, FarePolicyProgressiveDetailsBaseFare, FarePolicyProgressiveDetailsDeadKmFare, FarePolicyProgressiveDetailsNightShiftCharge, FarePolicyProgressiveDetailsPerExtraKmFare, FarePolicyProgressiveDetailsTId), FarePolicyProgressiveDetailsT (..))
import Storage.Tabular.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab ()
-- import Storage.Tabular.FarePolicy.Instances
import qualified Storage.Tabular.VechileNew as VN

-- findAllByMerchantId ::
--   Transactionable m =>
--   Id Merchant ->
--   m [FarePolicy]
-- findAllByMerchantId merchantId = buildDType $ do
--   res <- Esq.findAll' $ do
--     farePolicy <- from $ table @FarePolicyT
--     where_ $
--       farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
--     return farePolicy
--   catMaybes <$> mapM buildFullFarePolicy res

findAllByMerchantId :: (L.MonadFlow m) => Id Merchant -> m [FarePolicy]
findAllByMerchantId (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      result <- KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamFP.merchantId $ Se.Eq merchantId]
      case result of
        Right x -> traverse transformBeamFarePolicyToDomain x
        Left _ -> pure []
    Nothing -> pure []

-- findByMerchantIdAndVariant ::
--   Transactionable m =>
--   Id Merchant ->
--   Variant ->
--   m (Maybe FarePolicy)
-- findByMerchantIdAndVariant merchantId variant = buildDType $ do
--   res <- Esq.findOne' $ do
--     farePolicy <- from $ table @FarePolicyT
--     where_ $
--       farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
--         &&. farePolicy ^. FarePolicyVehicleVariant ==. val variant
--     return farePolicy
--   join <$> mapM buildFullFarePolicy res

findByMerchantIdAndVariant :: (L.MonadFlow m) => Id Merchant -> Variant -> m (Maybe FarePolicy)
findByMerchantIdAndVariant (Id merchantId) variant = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamFP.merchantId $ Se.Eq merchantId, Se.Is BeamFP.vehicleVariant $ Se.Eq variant]
      case result of
        Left _ -> pure Nothing
        Right x -> mapM transformBeamFarePolicyToDomain x
    Nothing -> pure Nothing

-- findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
-- findById farePolicyId = buildDType $ do
--   res <- Esq.findById' farePolicyId
--   join <$> mapM buildFullFarePolicy res

findById :: (L.MonadFlow m) => Id FarePolicy -> m (Maybe FarePolicy)
findById (Id farePolicyId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamFP.id $ Se.Eq farePolicyId]
      case result of
        Left _ -> pure Nothing
        Right x -> mapM transformBeamFarePolicyToDomain x
    Nothing -> pure Nothing

update :: (L.MonadFlow m, MonadTime m) => FarePolicy -> m ()
update farePolicy = do
  now <- getCurrentTime
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      _ <-
        KV.updateWoReturningWithKVConnector
          dbConf'
          VN.meshConfig
          [ Se.Set BeamFP.driverMinExtraFee $ Domain.minFee <$> farePolicy.driverExtraFeeBounds,
            Se.Set BeamFP.driverMaxExtraFee $ Domain.maxFee <$> farePolicy.driverExtraFeeBounds,
            Se.Set BeamFP.nightShiftStart $ Domain.nightShiftStart <$> farePolicy.nightShiftBounds,
            Se.Set BeamFP.nightShiftEnd $ Domain.nightShiftStart <$> farePolicy.nightShiftBounds,
            Se.Set BeamFP.updatedAt now
          ]
          [Se.Is BeamFP.id (Se.Eq $ getId farePolicy.id)]
      case farePolicy.farePolicyDetails of
        ProgressiveDetails fPPD -> do
          void $
            KV.updateWoReturningWithKVConnector
              dbConf'
              VN.meshConfig
              [ Se.Set BeamFPPD.baseFare $ fPPD.baseFare,
                Se.Set BeamFPPD.baseDistance $ fPPD.baseDistance,
                Se.Set BeamFPPD.perExtraKmFare $ fPPD.perExtraKmFare,
                Se.Set BeamFPPD.deadKmFare $ fPPD.deadKmFare,
                Se.Set BeamFPPD.nightShiftCharge $ fPPD.nightShiftCharge
              ]
              [Se.Is BeamFPPD.farePolicyId (Se.Eq $ getId farePolicy.id)]
        -- SlabsDetails (slabs :: (FPSlabsDetailsD 'Safe)) -> pure ()
        -- SlabsDetails (slabs :: (FPSlabsDetailsD Safe)) -> do
        SlabsDetails (FPSlabsDetails slabs) -> do
          _ <- QFPSlabDetSlabs.deleteAll'' farePolicy.id
          mapM_ (create'' farePolicy.id) slabs
    Nothing -> pure ()
  where
    -- create'' :: L.MonadFlow m => Id FarePolicy -> FPSlabsDetailsSlab -> m ()
    create'' :: L.MonadFlow m => Id FarePolicy -> FPSlabsDetailsSlab -> m ()
    create'' id' slab = do
      dbConf <- L.getOption Extra.EulerPsqlDbCfg
      case dbConf of
        Just dbConf' ->
          void $ KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainFarePolicyProgressiveDetailsToBeam' (id', slab))
        Nothing -> pure ()

    transformDomainFarePolicyProgressiveDetailsToBeam' :: (Id farePolicyId, FPSlabsDetailsSlab) -> BeamFPSS.FarePolicySlabsDetailsSlab
    transformDomainFarePolicyProgressiveDetailsToBeam' (Id farePolicyId, FPSlabsDetailsSlab {..}) =
      BeamFPSS.FarePolicySlabsDetailsSlabT
        { farePolicyId = farePolicyId,
          startDistance = startDistance,
          baseFare = baseFare,
          waitingCharge = waitingCharge,
          nightShiftCharge = nightShiftCharge
        }

-- update :: FarePolicy -> SqlDB ()
-- update farePolicy = do
--   now <- getCurrentTime
--   withFullEntity farePolicy $ \(FarePolicyT {..}, fpDetailsT) -> do
--     void $
--       Esq.update' $ \tbl -> do
--         set
--           tbl
--           [ FarePolicyDriverMinExtraFee =. val driverMinExtraFee,
--             FarePolicyDriverMaxExtraFee =. val driverMaxExtraFee,
--             FarePolicyNightShiftStart =. val nightShiftStart,
--             FarePolicyNightShiftEnd =. val nightShiftEnd,
--             FarePolicyUpdatedAt =. val now
--           ]
--         where_ $ tbl ^. FarePolicyTId ==. val (toKey farePolicy.id)

--     case fpDetailsT of
--       ProgressiveDetailsT fpdd -> updateProgressiveDetails fpdd
--       SlabsDetailsT fsdd -> updateSlabsDetails fsdd
--   where
--     updateProgressiveDetails FarePolicyProgressiveDetailsT {..} = do
--       void $
--         Esq.update' $ \tbl -> do
--           set
--             tbl
--             [ FarePolicyProgressiveDetailsBaseFare =. val baseFare,
--               FarePolicyProgressiveDetailsBaseDistance =. val baseDistance,
--               FarePolicyProgressiveDetailsPerExtraKmFare =. val perExtraKmFare,
--               FarePolicyProgressiveDetailsDeadKmFare =. val deadKmFare,
--               FarePolicyProgressiveDetailsNightShiftCharge =. val nightShiftCharge
--             ]
--           where_ $ tbl ^. FarePolicyProgressiveDetailsTId ==. val (toKey farePolicy.id)
--     updateSlabsDetails dets = do
--       QFPSlabDetSlabs.deleteAll' farePolicy.id
--       Esq.createMany' dets

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
