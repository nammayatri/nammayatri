{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AppFeedbackResponse where

import qualified Data.Aeson
import qualified Domain.Types.AppFeedbackResponse
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Lib.Feedback.Types
import qualified Sequelize as Se
import qualified Storage.Beam.AppFeedbackResponse as Beam
import qualified Storage.Queries.Transformers.AppFeedbackResponse

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AppFeedbackResponse.AppFeedbackResponse -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AppFeedbackResponse.AppFeedbackResponse] -> m ())
createMany = traverse_ create

findLatestByPersonId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.AppFeedbackResponse.AppFeedbackResponse]))
findLatestByPersonId limit offset personId = do findAllWithOptionsKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)] (Se.Desc Beam.createdAt) limit offset

updateActionResults ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe [Lib.Feedback.Types.FeedbackActionResult] -> Kernel.Types.Id.Id Domain.Types.AppFeedbackResponse.AppFeedbackResponse -> m ())
updateActionResults actionResults id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.actionResults (Data.Aeson.toJSON <$> actionResults), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.AppFeedbackResponse.AppFeedbackResponse -> m (Maybe Domain.Types.AppFeedbackResponse.AppFeedbackResponse))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AppFeedbackResponse.AppFeedbackResponse -> m ())
updateByPrimaryKey (Domain.Types.AppFeedbackResponse.AppFeedbackResponse {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actionResults (Data.Aeson.toJSON <$> actionResults),
      Se.Set Beam.answerMediaFileIds (answer >>= (.mediaFileIds)),
      Se.Set Beam.answerNumber (answer >>= (.number)),
      Se.Set Beam.answerOptionKeys (answer >>= (.selectedOptionKeys)),
      Se.Set Beam.answerRating (answer >>= (.rating)),
      Se.Set Beam.answerText (answer >>= (.text)),
      Se.Set Beam.answerScore answerScore,
      Se.Set Beam.configId (Kernel.Types.Id.getId configId),
      Se.Set Beam.configPilotVersions configPilotVersions,
      Se.Set Beam.logicVersion logicVersion,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.placement placement,
      Se.Set Beam.questionKey questionKey,
      Se.Set Beam.questionType questionType,
      Se.Set Beam.status status,
      Se.Set Beam.subjectId subjectId,
      Se.Set Beam.subjectType subjectType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.AppFeedbackResponse Domain.Types.AppFeedbackResponse.AppFeedbackResponse where
  fromTType' (Beam.AppFeedbackResponseT {..}) = do
    pure $
      Just
        Domain.Types.AppFeedbackResponse.AppFeedbackResponse
          { actionResults = Kernel.Utils.JSON.valueToMaybe =<< actionResults,
            answer = Storage.Queries.Transformers.AppFeedbackResponse.mkAnswer answerOptionKeys answerText answerNumber answerRating answerMediaFileIds,
            answerScore = answerScore,
            configId = Kernel.Types.Id.Id configId,
            configPilotVersions = configPilotVersions,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            logicVersion = logicVersion,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            placement = placement,
            questionKey = questionKey,
            questionType = questionType,
            status = status,
            subjectId = subjectId,
            subjectType = subjectType,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AppFeedbackResponse Domain.Types.AppFeedbackResponse.AppFeedbackResponse where
  toTType' (Domain.Types.AppFeedbackResponse.AppFeedbackResponse {..}) = do
    Beam.AppFeedbackResponseT
      { Beam.actionResults = Data.Aeson.toJSON <$> actionResults,
        Beam.answerMediaFileIds = answer >>= (.mediaFileIds),
        Beam.answerNumber = answer >>= (.number),
        Beam.answerOptionKeys = answer >>= (.selectedOptionKeys),
        Beam.answerRating = answer >>= (.rating),
        Beam.answerText = answer >>= (.text),
        Beam.answerScore = answerScore,
        Beam.configId = Kernel.Types.Id.getId configId,
        Beam.configPilotVersions = configPilotVersions,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.logicVersion = logicVersion,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.placement = placement,
        Beam.questionKey = questionKey,
        Beam.questionType = questionType,
        Beam.status = status,
        Beam.subjectId = subjectId,
        Beam.subjectType = subjectType,
        Beam.updatedAt = updatedAt
      }
