{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.CommunicationEngine.Storage.Queries.Communication (module Lib.CommunicationEngine.Storage.Queries.Communication, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.CommunicationEngine.Domain.Types.Communication
import qualified Lib.CommunicationEngine.Storage.Beam.BeamFlow
import qualified Lib.CommunicationEngine.Storage.Beam.Communication as Beam
import Lib.CommunicationEngine.Storage.Queries.CommunicationExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.CommunicationEngine.Domain.Types.Communication.Communication -> m ())
create = createWithKV

createMany :: (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.CommunicationEngine.Domain.Types.Communication.Communication] -> m ())
createMany = traverse_ create

findById ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.CommunicationEngine.Domain.Types.Communication.Communication -> m (Maybe Lib.CommunicationEngine.Domain.Types.Communication.Communication))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findBySenderIdAndStatus ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Lib.CommunicationEngine.Domain.Types.Communication.CommunicationStatus -> m ([Lib.CommunicationEngine.Domain.Types.Communication.Communication]))
findBySenderIdAndStatus senderId status = do findAllWithKV [Se.And [Se.Is Beam.senderId $ Se.Eq senderId, Se.Is Beam.status $ Se.Eq status]]

updateStatusById ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.CommunicationEngine.Domain.Types.Communication.CommunicationStatus -> Kernel.Types.Id.Id Lib.CommunicationEngine.Domain.Types.Communication.Communication -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.CommunicationEngine.Domain.Types.Communication.Communication -> m (Maybe Lib.CommunicationEngine.Domain.Types.Communication.Communication))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
