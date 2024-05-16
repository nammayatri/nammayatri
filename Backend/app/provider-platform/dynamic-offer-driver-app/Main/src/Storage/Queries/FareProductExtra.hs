{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareProductExtra where

import qualified Domain.Action.UI.FareProduct as Domain
import Domain.Types.Common
import Domain.Types.FareProduct
import qualified Domain.Types.FareProduct as Domain
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.ServiceTierType as DVST
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
import qualified Storage.Beam.FareProduct as BeamFP
import Storage.Queries.OrphanInstances.FareProduct

-- Extra code goes here --

findUnboundedByMerchantOpCityIdVariantArea ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  TripCategory ->
  DVST.ServiceTierType ->
  SL.Area ->
  m (Maybe Domain.FareProduct)
findUnboundedByMerchantOpCityIdVariantArea (Id merchantOpCityId) tripCategory serviceTier area =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamFP.merchantOperatingCityId $ Se.Eq merchantOpCityId,
          Se.Is BeamFP.area $ Se.Eq area,
          Se.Is BeamFP.vehicleVariant $ Se.Eq serviceTier,
          Se.Is BeamFP.tripCategory $ Se.Eq tripCategory,
          Se.Is BeamFP.timeBounds $ Se.Eq Domain.Unbounded,
          Se.Is BeamFP.enabled $ Se.Eq True
        ]
    ]
