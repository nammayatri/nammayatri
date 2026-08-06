{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.RsfUtrState (module Lib.Finance.Storage.Queries.RsfUtrState, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.RsfUtrState
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.RsfUtrState as Beam
import Lib.Finance.Storage.Queries.RsfUtrStateExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.RsfUtrState.RsfUtrState -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.RsfUtrState.RsfUtrState] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Lib.Finance.Domain.Types.RsfUtrState.RsfUtrState))
findByPrimaryKey merchantId utr = do findOneWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq merchantId, Se.Is Beam.utr $ Se.Eq utr]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.RsfUtrState.RsfUtrState -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.RsfUtrState.RsfUtrState {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.lastReportedAt lastReportedAt,
      Se.Set Beam.lastReportedMessageId lastReportedMessageId,
      Se.Set Beam.reportedDiff reportedDiff,
      Se.Set Beam.reportedStatus reportedStatus,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.merchantId $ Se.Eq merchantId, Se.Is Beam.utr $ Se.Eq utr]]
