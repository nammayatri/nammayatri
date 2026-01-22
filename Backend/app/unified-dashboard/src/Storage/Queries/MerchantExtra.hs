{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantExtra where

import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant as Beam
import Storage.Queries.OrphanInstances.Merchant

-- Extra code goes here --

findAllMerchants :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [Domain.Types.Merchant.Merchant]
findAllMerchants = findAllWithKV [Se.Is Beam.id $ Se.Not $ Se.Eq ""]

findAllMerchants' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Int -> Int -> m [Domain.Types.Merchant.Merchant]
findAllMerchants' limit offset = do
  findAllWithOptionsKV
    [Se.Is Beam.id $ Se.Not $ Se.Eq ""]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)
