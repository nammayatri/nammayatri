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
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.QuestionModuleMapping as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.QuestionModuleMapping.QuestionModuleMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.QuestionModuleMapping.QuestionModuleMapping] -> m ())
createMany = traverse_ create

findAllWithModuleId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> m [Domain.Types.QuestionModuleMapping.QuestionModuleMapping])
findAllWithModuleId moduleId = do findAllWithKV [Se.Is Beam.moduleId $ Se.Eq (Kernel.Types.Id.getId moduleId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> Kernel.Types.Id.Id Domain.Types.QuestionModuleMapping.QuestionModuleMapping -> m (Maybe Domain.Types.QuestionModuleMapping.QuestionModuleMapping))
findByPrimaryKey moduleId questionId = do findOneWithKV [Se.And [Se.Is Beam.moduleId $ Se.Eq (Kernel.Types.Id.getId moduleId), Se.Is Beam.questionId $ Se.Eq (Kernel.Types.Id.getId questionId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.QuestionModuleMapping.QuestionModuleMapping -> m ())
updateByPrimaryKey (Domain.Types.QuestionModuleMapping.QuestionModuleMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.quizCoinFunction quizCoinFunction, Se.Set Beam.updatedAt _now]
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
            quizCoinFunction = quizCoinFunction,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.QuestionModuleMapping Domain.Types.QuestionModuleMapping.QuestionModuleMapping where
  toTType' (Domain.Types.QuestionModuleMapping.QuestionModuleMapping {..}) = do
    Beam.QuestionModuleMappingT
      { Beam.moduleId = Kernel.Types.Id.getId moduleId,
        Beam.questionId = Kernel.Types.Id.getId questionId,
        Beam.quizCoinFunction = quizCoinFunction,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
