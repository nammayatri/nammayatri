{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.BusinessHourExtra where

import Domain.Types.BusinessHour
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.BusinessHour as Beam
import Storage.Queries.OrphanInstances.BusinessHour ()

-- Extra code goes here --
findAllByIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Id BusinessHour] -> m [BusinessHour]
findAllByIds ids = findAllWithKV [Se.Is Beam.id $ Se.In (map getId ids)]

findByBtypeAndId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => BusinessHourType -> [Id BusinessHour] -> m (Maybe BusinessHour)
findByBtypeAndId btype id = findOneWithKV [Se.And [Se.Is Beam.btype $ Se.Eq btype, Se.Is Beam.id $ Se.In (map getId id)]]
