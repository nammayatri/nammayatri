{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Utils.Storage.Queries.VoipCallStatus (module Lib.Utils.Storage.Queries.VoipCallStatus, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Utils.Storage.Beam.BeamFlow
import qualified Lib.Utils.Storage.Beam.VoipCallStatus as Beam
import Lib.Utils.Storage.Queries.VoipCallStatusExtra as ReExport
import qualified Lib.Utils.Types.VoipCallStatus
import qualified Sequelize as Se
import qualified Utils.Common.Voip.Types.VoipStorageType

create :: (Lib.Utils.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Utils.Types.VoipCallStatus.VoipCallStatus -> m ())
create = createWithKV

createMany :: (Lib.Utils.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Utils.Types.VoipCallStatus.VoipCallStatus] -> m ())
createMany = traverse_ create

updateByCallId ::
  (Lib.Utils.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Utils.Common.Voip.Types.VoipStorageType.VoipStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ())
updateByCallId callStatus errorCode networkType networkQuality callId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.callStatus callStatus,
      Se.Set Beam.errorCode errorCode,
      Se.Set Beam.networkType networkType,
      Se.Set Beam.networkQuality networkQuality,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.callId $ Se.Eq callId]

findByPrimaryKey :: (Lib.Utils.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Utils.Types.VoipCallStatus.VoipCallStatus -> m (Maybe Lib.Utils.Types.VoipCallStatus.VoipCallStatus))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Utils.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Utils.Types.VoipCallStatus.VoipCallStatus -> m ())
updateByPrimaryKey (Lib.Utils.Types.VoipCallStatus.VoipCallStatus {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.callId callId,
      Se.Set Beam.callStatus callStatus,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.errorCode errorCode,
      Se.Set Beam.merchantCity merchantCity,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.networkQuality networkQuality,
      Se.Set Beam.networkType networkType,
      Se.Set Beam.rideId (Kernel.Types.Id.getId rideId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
