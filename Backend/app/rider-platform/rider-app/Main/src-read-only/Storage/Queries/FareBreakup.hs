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
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareBreakup as Beam
import Storage.Queries.FareBreakupExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.FareBreakup.FareBreakup -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.FareBreakup.FareBreakup] -> m ())
createMany = traverse_ create

findAllByEntityIdAndEntityType :: KvDbFlow m r => (Kernel.Prelude.Text -> Domain.Types.FareBreakup.FareBreakupEntityType -> m [Domain.Types.FareBreakup.FareBreakup])
findAllByEntityIdAndEntityType entityId entityType = do findAllWithKV [Se.And [Se.Is Beam.bookingId $ Se.Eq entityId, Se.Is Beam.entityType $ Se.Eq entityType]]

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.FareBreakup.FareBreakup -> m (Maybe Domain.Types.FareBreakup.FareBreakup))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]
