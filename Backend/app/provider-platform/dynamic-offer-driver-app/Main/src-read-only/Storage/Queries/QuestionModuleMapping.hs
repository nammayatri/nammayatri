{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.QuestionModuleMapping where

import qualified Domain.Types.LmsModule
import qualified Domain.Types.QuestionModuleMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.QuestionModuleMapping as Beam

create :: KvDbFlow m r => (Domain.Types.QuestionModuleMapping.QuestionModuleMapping -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.QuestionModuleMapping.QuestionModuleMapping] -> m ())
createMany = traverse_ create

findAllWithModuleId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> m [Domain.Types.QuestionModuleMapping.QuestionModuleMapping])
findAllWithModuleId (Kernel.Types.Id.Id moduleId) = do findAllWithKV [Se.Is Beam.moduleId $ Se.Eq moduleId]

findByPrimaryKey ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> Kernel.Types.Id.Id Domain.Types.QuestionModuleMapping.QuestionModuleMapping -> m (Maybe Domain.Types.QuestionModuleMapping.QuestionModuleMapping))
findByPrimaryKey (Kernel.Types.Id.Id moduleId) (Kernel.Types.Id.Id questionId) = do findOneWithKV [Se.And [Se.Is Beam.moduleId $ Se.Eq moduleId, Se.Is Beam.questionId $ Se.Eq questionId]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.QuestionModuleMapping.QuestionModuleMapping -> m ())
updateByPrimaryKey (Domain.Types.QuestionModuleMapping.QuestionModuleMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.createdAt createdAt, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.moduleId $ Se.Eq (Kernel.Types.Id.getId moduleId),
          Se.Is Beam.questionId $ Se.Eq (Kernel.Types.Id.getId questionId)
        ]
    ]

instance FromTType' Beam.QuestionModuleMapping Domain.Types.QuestionModuleMapping.QuestionModuleMapping where
  fromTType' (Beam.QuestionModuleMappingT {..}) = do
    pure $
      Just
        Domain.Types.QuestionModuleMapping.QuestionModuleMapping
          { moduleId = Kernel.Types.Id.Id moduleId,
            questionId = Kernel.Types.Id.Id questionId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.QuestionModuleMapping Domain.Types.QuestionModuleMapping.QuestionModuleMapping where
  toTType' (Domain.Types.QuestionModuleMapping.QuestionModuleMapping {..}) = do
    Beam.QuestionModuleMappingT
      { Beam.moduleId = Kernel.Types.Id.getId moduleId,
        Beam.questionId = Kernel.Types.Id.getId questionId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
