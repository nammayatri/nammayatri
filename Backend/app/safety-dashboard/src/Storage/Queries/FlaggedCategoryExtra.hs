{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FlaggedCategoryExtra where

import qualified Domain.Types.FlaggedCategory
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.FlaggedCategory as Beam
import Storage.Queries.OrphanInstances.FlaggedCategory ()

-- Extra code goes here --

findAll :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Int -> Int -> m [Domain.Types.FlaggedCategory.FlaggedCategory]
findAll limit offset = do
  findAllWithOptionsKV
    [ Se.Is Beam.id $ Se.Not $ Se.Eq ""
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

countAll :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m Int
countAll = do
  res <- findAllWithKV [Se.Is Beam.id $ Se.Not $ Se.Eq ""]
  pure $ length res
