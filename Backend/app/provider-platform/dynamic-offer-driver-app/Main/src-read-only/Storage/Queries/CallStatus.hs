{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CallStatus (module Storage.Queries.CallStatus, module ReExport) where

import qualified Domain.Types.CallStatus
import Kernel.Beam.Functions
import qualified Kernel.External.Call.Interface.Types
import qualified Kernel.External.Call.Types
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CallStatus as Beam
import Storage.Queries.CallStatusExtra as ReExport

createMany :: KvDbFlow m r => ([Domain.Types.CallStatus.CallStatus] -> m ())
createMany = traverse_ create

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus -> m (Maybe Domain.Types.CallStatus.CallStatus))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

updateCallError ::
  KvDbFlow m r =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Call.Types.CallService -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus -> m ())
updateCallError callError callService merchantId (Kernel.Types.Id.Id id) = do
  updateWithKV
    [ Se.Set Beam.callError callError,
      Se.Set Beam.callService callService,
      Se.Set Beam.merchantId merchantId
    ]
    [Se.Is Beam.id $ Se.Eq id]

updateCallStatus ::
  KvDbFlow m r =>
  (Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.External.Call.Interface.Types.CallStatus -> Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus -> m ())
updateCallStatus conversationDuration recordingUrl status (Kernel.Types.Id.Id id) = do
  updateWithKV
    [ Se.Set Beam.conversationDuration conversationDuration,
      Se.Set Beam.recordingUrl recordingUrl,
      Se.Set Beam.status status
    ]
    [Se.Is Beam.id $ Se.Eq id]

updateCallStatusWithRideId ::
  KvDbFlow m r =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Call.Types.CallService -> Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus -> m ())
updateCallStatusWithRideId entityId dtmfNumberUsed merchantId callService (Kernel.Types.Id.Id id) = do
  updateWithKV
    [ Se.Set Beam.entityId entityId,
      Se.Set Beam.dtmfNumberUsed dtmfNumberUsed,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.callService callService
    ]
    [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus -> m (Maybe Domain.Types.CallStatus.CallStatus))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.CallStatus.CallStatus -> m ())
updateByPrimaryKey (Domain.Types.CallStatus.CallStatus {..}) = do
  updateWithKV
    [ Se.Set Beam.callError callError,
      Se.Set Beam.callId callId,
      Se.Set Beam.callService callService,
      Se.Set Beam.conversationDuration conversationDuration,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.dtmfNumberUsed dtmfNumberUsed,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.recordingUrl recordingUrl,
      Se.Set Beam.status status
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
