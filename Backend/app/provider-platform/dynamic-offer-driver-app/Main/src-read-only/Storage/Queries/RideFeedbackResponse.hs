{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RideFeedbackResponse where

import qualified Data.Aeson
import qualified Domain.Types.RideFeedbackResponse
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Sequelize as Se
import qualified Storage.Beam.RideFeedbackResponse as Beam
import qualified Storage.Queries.Transformers.RideFeedbackResponse

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RideFeedbackResponse.RideFeedbackResponse] -> m ())
createMany = traverse_ create

findAllByTransactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.RideFeedbackResponse.RideFeedbackResponse]))
findAllByTransactionId transactionId = do findAllWithKV [Se.Is Beam.transactionId $ Se.Eq transactionId]

updateActionResults ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe [Domain.Types.RideFeedbackResponse.RideFeedbackActionResult] -> Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> m ())
updateActionResults actionResults id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.actionResults (Data.Aeson.toJSON <$> actionResults), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> m (Maybe Domain.Types.RideFeedbackResponse.RideFeedbackResponse))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> m ())
updateByPrimaryKey (Domain.Types.RideFeedbackResponse.RideFeedbackResponse {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actionResults (Data.Aeson.toJSON <$> actionResults),
      Se.Set Beam.answerMediaFileIds (answer >>= (.mediaFileIds)),
      Se.Set Beam.answerNumber (answer >>= (.number)),
      Se.Set Beam.answerOptionKeys (answer >>= (.selectedOptionKeys)),
      Se.Set Beam.answerRating (answer >>= (.rating)),
      Se.Set Beam.answerText (answer >>= (.text)),
      Se.Set Beam.answerScore answerScore,
      Se.Set Beam.bookingId (Kernel.Types.Id.getId bookingId),
      Se.Set Beam.configId (Kernel.Types.Id.getId configId),
      Se.Set Beam.configPilotVersions configPilotVersions,
      Se.Set Beam.driverId (Kernel.Types.Id.getId <$> driverId),
      Se.Set Beam.logicVersion logicVersion,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.questionKey questionKey,
      Se.Set Beam.questionType questionType,
      Se.Set Beam.rideId (Kernel.Types.Id.getId <$> rideId),
      Se.Set Beam.secondsIntoRide secondsIntoRide,
      Se.Set Beam.secondsSinceBooking secondsSinceBooking,
      Se.Set Beam.stage stage,
      Se.Set Beam.status status,
      Se.Set Beam.subjectId subjectId,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.RideFeedbackResponse Domain.Types.RideFeedbackResponse.RideFeedbackResponse where
  fromTType' (Beam.RideFeedbackResponseT {..}) = do
    pure $
      Just
        Domain.Types.RideFeedbackResponse.RideFeedbackResponse
          { actionResults = Kernel.Utils.JSON.valueToMaybe =<< actionResults,
            answer = Storage.Queries.Transformers.RideFeedbackResponse.mkAnswer answerOptionKeys answerText answerNumber answerRating answerMediaFileIds,
            answerScore = answerScore,
            bookingId = Kernel.Types.Id.Id bookingId,
            configId = Kernel.Types.Id.Id configId,
            configPilotVersions = configPilotVersions,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id <$> driverId,
            id = Kernel.Types.Id.Id id,
            logicVersion = logicVersion,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            questionKey = questionKey,
            questionType = questionType,
            rideId = Kernel.Types.Id.Id <$> rideId,
            secondsIntoRide = secondsIntoRide,
            secondsSinceBooking = secondsSinceBooking,
            stage = stage,
            status = status,
            subjectId = subjectId,
            transactionId = transactionId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RideFeedbackResponse Domain.Types.RideFeedbackResponse.RideFeedbackResponse where
  toTType' (Domain.Types.RideFeedbackResponse.RideFeedbackResponse {..}) = do
    Beam.RideFeedbackResponseT
      { Beam.actionResults = Data.Aeson.toJSON <$> actionResults,
        Beam.answerMediaFileIds = answer >>= (.mediaFileIds),
        Beam.answerNumber = answer >>= (.number),
        Beam.answerOptionKeys = answer >>= (.selectedOptionKeys),
        Beam.answerRating = answer >>= (.rating),
        Beam.answerText = answer >>= (.text),
        Beam.answerScore = answerScore,
        Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.configId = Kernel.Types.Id.getId configId,
        Beam.configPilotVersions = configPilotVersions,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId <$> driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.logicVersion = logicVersion,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.questionKey = questionKey,
        Beam.questionType = questionType,
        Beam.rideId = Kernel.Types.Id.getId <$> rideId,
        Beam.secondsIntoRide = secondsIntoRide,
        Beam.secondsSinceBooking = secondsSinceBooking,
        Beam.stage = stage,
        Beam.status = status,
        Beam.subjectId = subjectId,
        Beam.transactionId = transactionId,
        Beam.updatedAt = updatedAt
      }
