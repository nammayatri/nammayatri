{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BusinessHourExtra where

import Domain.Types.BusinessHour
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BusinessHour as Beam
import Storage.Queries.OrphanInstances.BusinessHour

-- Extra code goes here --
findByBtypeAndId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => BusinessHourType -> [Id BusinessHour] -> m (Maybe BusinessHour)
findByBtypeAndId btype id = findOneWithKV [Se.And [Se.Is Beam.btype $ Se.Eq btype, Se.Is Beam.id $ Se.In (map getId id)]]
