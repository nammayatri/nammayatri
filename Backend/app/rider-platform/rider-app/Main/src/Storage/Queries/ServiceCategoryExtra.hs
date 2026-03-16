{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.ServiceCategoryExtra where

import Domain.Types.ServiceCategory
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Sequelize as Se
import qualified Storage.Beam.ServiceCategory as Beam
import Storage.Queries.OrphanInstances.ServiceCategory ()

-- Extra code goes here --
findByIdAndName :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Id ServiceCategory] -> Text -> m (Maybe ServiceCategory)
findByIdAndName id name = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.In (map getId id), Se.Is Beam.name $ Se.Eq name]]
