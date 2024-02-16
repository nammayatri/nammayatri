{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.QuestionInformation where

import qualified Domain.Types.QuestionInformation
import qualified Domain.Types.QuestionModuleMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.QuestionInformation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.QuestionInformation.QuestionInformation -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.QuestionInformation.QuestionInformation] -> m ()
createMany = traverse_ create

findByIdAndLanguage :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.QuestionModuleMapping.QuestionModuleMapping -> Kernel.External.Types.Language -> m (Maybe (Domain.Types.QuestionInformation.QuestionInformation))
findByIdAndLanguage (Kernel.Types.Id.Id questionId) language = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.questionId $ Se.Eq questionId,
          Se.Is Beam.language $ Se.Eq language
        ]
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.External.Types.Language -> Kernel.Types.Id.Id Domain.Types.QuestionModuleMapping.QuestionModuleMapping -> m (Maybe (Domain.Types.QuestionInformation.QuestionInformation))
findByPrimaryKey language (Kernel.Types.Id.Id questionId) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.language $ Se.Eq language,
          Se.Is Beam.questionId $ Se.Eq questionId
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.QuestionInformation.QuestionInformation -> m ()
updateByPrimaryKey Domain.Types.QuestionInformation.QuestionInformation {..} = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.options options,
      Se.Set Beam.question question,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.language $ Se.Eq language,
          Se.Is Beam.questionId $ Se.Eq (Kernel.Types.Id.getId questionId)
        ]
    ]

instance FromTType' Beam.QuestionInformation Domain.Types.QuestionInformation.QuestionInformation where
  fromTType' Beam.QuestionInformationT {..} = do
    pure $
      Just
        Domain.Types.QuestionInformation.QuestionInformation
          { language = language,
            options = options,
            question = question,
            questionId = Kernel.Types.Id.Id questionId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.QuestionInformation Domain.Types.QuestionInformation.QuestionInformation where
  toTType' Domain.Types.QuestionInformation.QuestionInformation {..} = do
    Beam.QuestionInformationT
      { Beam.language = language,
        Beam.options = options,
        Beam.question = question,
        Beam.questionId = Kernel.Types.Id.getId questionId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
