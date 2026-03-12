{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.KnowledgeCenterExtra where

import qualified Domain.Types.KnowledgeCenter as DKC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.KnowledgeCenter as Beam
import Storage.Queries.OrphanInstances.KnowledgeCenter

-- | Find all knowledge center documents for a merchant operating city (for sop/list grouped by sopType).
findAllByMerchantOperatingCityId ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [DKC.KnowledgeCenter]
findAllByMerchantOperatingCityId mocId =
  findAllWithOptionsKV
    [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just mocId.getId)]
    (Se.Desc Beam.createdAt)
    (Just 10000)
    (Just 0)
