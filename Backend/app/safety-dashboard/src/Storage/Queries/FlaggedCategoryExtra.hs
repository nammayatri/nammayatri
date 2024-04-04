{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FlaggedCategoryExtra where

import qualified Domain.Types.FlaggedCategory
import qualified Domain.Types.Suspect as DS
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FlaggedCategory as Beam
import Storage.Queries.OrphanInstances.FlaggedCategory

-- Extra code goes here --

findAll :: KvDbFlow m r => Int -> Int -> m [Domain.Types.FlaggedCategory.FlaggedCategory]
findAll limit offset = do
  findAllWithOptionsKV
    [ Se.Is Beam.id $ Se.Not $ Se.Eq ""
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

countAll :: KvDbFlow m r => m Int
countAll = do
  res <- findAllWithKV [Se.Is Beam.id $ Se.Not $ Se.Eq ""]
  pure $ length res
