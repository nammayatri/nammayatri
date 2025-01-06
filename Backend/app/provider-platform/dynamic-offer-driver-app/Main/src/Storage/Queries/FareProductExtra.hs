module Storage.Queries.FareProductExtra where

import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import Domain.Types.FarePolicy
import Domain.Types.FareProduct
import qualified Domain.Types.FareProduct as Domain
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as Domain
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation as SL
import qualified Sequelize as Se
import qualified Storage.Beam.FareProduct as Beam
import Storage.Queries.OrphanInstances.FareProduct ()

-- Extra code goes here --

delete :: (MonadFlow m, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FareProduct.FareProduct -> m ()
delete (Id id) = deleteWithKV [Se.And [Se.Is Beam.id (Se.Eq id)]]

updateFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id FarePolicy -> Kernel.Types.Id.Id Domain.Types.FareProduct.FareProduct -> m ()
updateFarePolicyId (Kernel.Types.Id.Id farePolicyId) (Kernel.Types.Id.Id id) = do updateWithKV [Se.Set Beam.farePolicyId farePolicyId] [Se.Is Beam.id $ Se.Eq id]

findAllBoundedByMerchantOpCityIdVariantArea' ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  SL.Area ->
  DVST.ServiceTierType ->
  DTC.TripCategory ->
  Domain.TimeBound ->
  Bool ->
  [Domain.SearchSource] ->
  m [Domain.FareProduct]
findAllBoundedByMerchantOpCityIdVariantArea' (Id merchantOperatingCityId) area vehicleServiceTier tripCategory timeBounds enabled searchSources = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.area $ Se.Eq area,
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleServiceTier,
          Se.Is Beam.tripCategory $ Se.Eq tripCategory,
          Se.Is Beam.searchSource $ Se.In searchSources,
          Se.Is Beam.timeBounds $ Se.Not $ Se.Eq timeBounds,
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]

findAllBoundedByMerchantOpCityIdVariantArea ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  SL.Area ->
  DVST.ServiceTierType ->
  DTC.TripCategory ->
  Domain.TimeBound ->
  Bool ->
  [Domain.SearchSource] ->
  m [Domain.FareProduct]
findAllBoundedByMerchantOpCityIdVariantArea (Id merchantOperatingCityId) area vehicleServiceTier tripCategory timeBounds enabled searchSources = do
  if isInterCityWithCity tripCategory
    then do
      fareProducts <- findAllBoundedByMerchantOpCityIdVariantArea' (Id merchantOperatingCityId) area vehicleServiceTier tripCategory timeBounds enabled searchSources
      if null fareProducts
        then findAllBoundedByMerchantOpCityIdVariantArea' (Id merchantOperatingCityId) area vehicleServiceTier (removeCityFromTripCategory tripCategory) timeBounds enabled searchSources
        else return fareProducts
    else findAllBoundedByMerchantOpCityIdVariantArea' (Id merchantOperatingCityId) area vehicleServiceTier tripCategory timeBounds enabled searchSources

findAllUnboundedFareProductForVariants' ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  SL.Area ->
  DTC.TripCategory ->
  Domain.TimeBound ->
  Kernel.Prelude.Bool ->
  [Domain.SearchSource] ->
  m [Domain.FareProduct]
findAllUnboundedFareProductForVariants' merchantOperatingCityId area tripCategory timeBounds enabled searchSource = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.area $ Se.Eq area,
          Se.Is Beam.tripCategory $ Se.Eq tripCategory,
          Se.Is Beam.timeBounds $ Se.Eq timeBounds,
          Se.Is Beam.enabled $ Se.Eq enabled,
          Se.Is Beam.searchSource $ Se.In searchSource
        ]
    ]

findAllUnboundedFareProductForVariants ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  SL.Area ->
  DTC.TripCategory ->
  Domain.TimeBound ->
  Kernel.Prelude.Bool ->
  [Domain.SearchSource] ->
  m [Domain.FareProduct]
findAllUnboundedFareProductForVariants (Id merchantOperatingCityId) area tripCategory timeBounds enabled searchSources = do
  if isInterCityWithCity tripCategory
    then do
      fareProducts <- findAllUnboundedFareProductForVariants' (Id merchantOperatingCityId) area tripCategory timeBounds enabled searchSources
      if null fareProducts
        then findAllUnboundedFareProductForVariants' (Id merchantOperatingCityId) area (removeCityFromTripCategory tripCategory) timeBounds enabled searchSources
        else return fareProducts
    else findAllUnboundedFareProductForVariants' (Id merchantOperatingCityId) area tripCategory timeBounds enabled searchSources

removeCityFromTripCategory :: DTC.TripCategory -> DTC.TripCategory
removeCityFromTripCategory (DTC.InterCity mode _) = DTC.InterCity mode Nothing
removeCityFromTripCategory (DTC.CrossCity mode _) = DTC.CrossCity mode Nothing
removeCityFromTripCategory x = x

isInterCityWithCity :: DTC.TripCategory -> Bool
isInterCityWithCity (DTC.InterCity _ (Just _)) = True
isInterCityWithCity (DTC.CrossCity _ (Just _)) = True
isInterCityWithCity _ = False
