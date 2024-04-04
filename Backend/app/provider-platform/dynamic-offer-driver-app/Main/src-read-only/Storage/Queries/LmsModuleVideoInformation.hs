{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.LmsModuleVideoInformation where

import qualified Domain.Types.LmsModule
import qualified Domain.Types.LmsModuleVideoInformation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.LmsModuleVideoInformation as Beam

create :: KvDbFlow m r => (Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation] -> m ())
createMany = traverse_ create

findByVideoId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> m (Maybe Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation))
findByVideoId (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

getAllVideos ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> [Domain.Types.LmsModuleVideoInformation.VideoStatus] -> m [Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation])
getAllVideos (Kernel.Types.Id.Id moduleId) videoStatus = do findAllWithKV [Se.And [Se.Is Beam.moduleId $ Se.Eq moduleId, Se.Is Beam.videoStatus $ Se.In videoStatus]]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> m (Maybe Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> m ())
updateByPrimaryKey (Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.moduleId (Kernel.Types.Id.getId moduleId),
      Se.Set Beam.rank rank,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.videoStatus videoStatus
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.LmsModuleVideoInformation Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation where
  fromTType' (Beam.LmsModuleVideoInformationT {..}) = do
    pure $
      Just
        Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            moduleId = Kernel.Types.Id.Id moduleId,
            rank = rank,
            updatedAt = updatedAt,
            videoStatus = videoStatus
          }

instance ToTType' Beam.LmsModuleVideoInformation Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation where
  toTType' (Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation {..}) = do
    Beam.LmsModuleVideoInformationT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.moduleId = Kernel.Types.Id.getId moduleId,
        Beam.rank = rank,
        Beam.updatedAt = updatedAt,
        Beam.videoStatus = videoStatus
      }
