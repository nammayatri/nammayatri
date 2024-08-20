{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareBreakup (module Storage.Queries.FareBreakup, module ReExport) where

import qualified Domain.Types.FareBreakup
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareBreakup as Beam
import Storage.Queries.FareBreakupExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareBreakup.FareBreakup -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FareBreakup.FareBreakup] -> m ())
createMany = traverse_ create

deleteByEntityIdAndEntityType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Domain.Types.FareBreakup.FareBreakupEntityType -> m ())
deleteByEntityIdAndEntityType entityId entityType = do deleteWithKV [Se.And [Se.Is Beam.bookingId $ Se.Eq entityId, Se.Is Beam.entityType $ Se.Eq entityType]]

findAllByEntityIdAndEntityType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Domain.Types.FareBreakup.FareBreakupEntityType -> m [Domain.Types.FareBreakup.FareBreakup])
findAllByEntityIdAndEntityType entityId entityType = do findAllWithKVAndConditionalDB [Se.And [Se.Is Beam.bookingId $ Se.Eq entityId, Se.Is Beam.entityType $ Se.Eq entityType]] Nothing

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FareBreakup.FareBreakup -> m (Maybe Domain.Types.FareBreakup.FareBreakup))
findById id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
