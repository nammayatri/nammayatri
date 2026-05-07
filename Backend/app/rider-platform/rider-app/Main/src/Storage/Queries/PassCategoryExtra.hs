{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PassCategoryExtra where

import qualified Data.List as DL
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PassCategory as DPassCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.PassCategory as Beam
import Storage.Queries.OrphanInstances.PassCategory ()

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPassCategory.PassCategory ->
  m (Maybe DPassCategory.PassCategory)
findById passCategoryId = findOneWithKV [Se.Is Beam.id $ Se.Eq (getId passCategoryId)]

sortPassCategoriesByOrder :: [DPassCategory.PassCategory] -> [DPassCategory.PassCategory]
sortPassCategoriesByOrder =
  DL.sortOn $ \pc ->
    case pc.order of
      Just n -> Left n
      Nothing -> Right ()

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [DPassCategory.PassCategory]
findAllByMerchantOperatingCityId merchantOperatingCityId = do
  categories <- findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId)]
  pure $ sortPassCategoriesByOrder categories
