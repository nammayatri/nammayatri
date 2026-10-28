{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.MgCampaignExtra where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MgCampaign as DMgC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.MgCampaign as BeamMgC
import Storage.Queries.OrphanInstances.MgCampaign ()

-- | One page of a city's campaigns, newest first. merchantOperatingCityId is not a KV secondary key
-- (deny-listed), so this runs against Postgres, which also does the ordering and the paging.
findAllByMerchantOperatingCityIdPaged :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Int -> Int -> m [DMgC.MgCampaign]
findAllByMerchantOperatingCityIdPaged merchantOperatingCityId limit offset =
  findAllWithOptionsKV
    [Se.Is BeamMgC.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId]
    (Se.Desc BeamMgC.createdAt)
    (Just limit)
    (Just offset)
