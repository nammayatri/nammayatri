{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareProductExtra where

import Domain.Types.Common
import Domain.Types.FareProduct
import qualified Domain.Types.FareProduct as Domain
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.TimeBound as Domain
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Types.SpecialLocation as SL
import qualified Sequelize as Se
import qualified Storage.Beam.FareProduct as Beam
import Storage.Queries.OrphanInstances.FareProduct

-- Extra code goes here --

findAllBoundedByMerchantOpCityIdVariantArea ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  SL.Area ->
  DVST.ServiceTierType ->
  TripCategory ->
  Domain.TimeBound ->
  Bool ->
  [Domain.SearchSource] ->
  m [Domain.FareProduct]
findAllBoundedByMerchantOpCityIdVariantArea (Id merchantOperatingCityId) area vehicleServiceTier tripCategory timeBounds enabled searchSources = do
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
