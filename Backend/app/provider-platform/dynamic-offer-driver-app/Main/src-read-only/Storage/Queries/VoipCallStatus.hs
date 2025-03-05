{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VoipCallStatus (module Storage.Queries.VoipCallStatus, module ReExport) where

import qualified Domain.Types.VoipCallStatus
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VoipCallStatus as Beam
import Storage.Queries.VoipCallStatusExtra as ReExport
import qualified Utils.Common.Voip.Types.VoipStorageType

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VoipCallStatus.VoipCallStatus -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VoipCallStatus.VoipCallStatus] -> m ())
createMany = traverse_ create

updateByCallId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
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

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VoipCallStatus.VoipCallStatus -> m (Maybe Domain.Types.VoipCallStatus.VoipCallStatus))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VoipCallStatus.VoipCallStatus -> m ())
updateByPrimaryKey (Domain.Types.VoipCallStatus.VoipCallStatus {..}) = do
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
