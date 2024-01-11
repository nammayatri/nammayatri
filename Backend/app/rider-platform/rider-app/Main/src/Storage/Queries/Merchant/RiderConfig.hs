{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.RiderConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant.RiderConfig (RiderConfig (..))
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.RiderConfig as BeamRC

findByMerchantOperatingCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe RiderConfig)
findByMerchantOperatingCityId (Id merchantOperatingCityId) = findOneWithKV [Se.Is BeamRC.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

instance FromTType' BeamRC.RiderConfig RiderConfig where
  fromTType' BeamRC.RiderConfigT {..} = do
    pure $
      Just
        RiderConfig
          { merchantOperatingCityId = Id merchantOperatingCityId,
            enableLocalPoliceSupport = enableLocalPoliceSupport,
            enableSupportForSafety = enableSupportForSafety,
            localPoliceNumber = localPoliceNumber,
            videoFileSizeUpperLimit = videoFileSizeUpperLimit,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamRC.RiderConfig RiderConfig where
  toTType' RiderConfig {..} = do
    BeamRC.RiderConfigT
      { BeamRC.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamRC.enableLocalPoliceSupport = enableLocalPoliceSupport,
        BeamRC.localPoliceNumber = localPoliceNumber,
        BeamRC.enableSupportForSafety = enableSupportForSafety,
        BeamRC.videoFileSizeUpperLimit = videoFileSizeUpperLimit,
        BeamRC.createdAt = createdAt,
        BeamRC.updatedAt = updatedAt
      }
