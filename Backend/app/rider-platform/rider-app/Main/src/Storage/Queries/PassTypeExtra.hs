{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PassTypeExtra where

import qualified Domain.Types.PassCategory as DPassCategory
import qualified Domain.Types.PassType as DPassType
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.PassType as Beam
import Storage.Queries.OrphanInstances.PassType ()

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPassType.PassType ->
  m (Maybe DPassType.PassType)
findById passTypeId = findOneWithKV [Se.Is Beam.id $ Se.Eq (getId passTypeId)]

findAllByPassCategoryId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPassCategory.PassCategory ->
  m [DPassType.PassType]
findAllByPassCategoryId passCategoryId = do
  findAllWithOptionsKV
    [Se.Is Beam.passCategoryId $ Se.Eq (getId passCategoryId)]
    (Se.Asc Beam.order)
    Nothing
    Nothing
