{-# OPTIONS_GHC -Wno-orphans #-}

-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverPoolConfigExtra where

-- Extra code goes here --

import qualified Domain.Types.DriverPoolConfig as Domain
import qualified Domain.Types.Extra.TimeBound as DTB
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverPoolConfig as BeamDPC
import Storage.Queries.OrphanInstances.DriverPoolConfig ()

findAllUnboundedByMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m [Domain.DriverPoolConfig]
findAllUnboundedByMerchantOpCityId (Id merchantOpCityId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDPC.merchantOperatingCityId $ Se.Eq merchantOpCityId,
          Se.Is BeamDPC.timeBounds $ Se.Eq DTB.Unbounded
        ]
    ]

findAllBoundedByMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m [Domain.DriverPoolConfig]
findAllBoundedByMerchantOpCityId (Id merchantOpCityId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDPC.merchantOperatingCityId $ Se.Eq merchantOpCityId,
          Se.Is BeamDPC.timeBounds $ Se.Not $ Se.Eq DTB.Unbounded
        ]
    ]
