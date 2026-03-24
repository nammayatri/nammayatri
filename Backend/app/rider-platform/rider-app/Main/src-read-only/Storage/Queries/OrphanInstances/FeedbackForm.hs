{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.FeedbackForm where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.FeedbackForm
import qualified Storage.Beam.FeedbackForm as Beam
import qualified Kernel.Types.Id
import qualified Data.Aeson
import qualified Kernel.Utils.JSON



instance FromTType' Beam.FeedbackForm Domain.Types.FeedbackForm.FeedbackForm
    where fromTType' (Beam.FeedbackFormT {..}) = do pure $ Just Domain.Types.FeedbackForm.FeedbackForm{answer = answer,
                                                                                                       answerType = answerType,
                                                                                                       badges = Kernel.Utils.JSON.valueToMaybe =<< badges,
                                                                                                       categoryName = categoryName,
                                                                                                       id = Kernel.Types.Id.Id id,
                                                                                                       question = question,
                                                                                                       questionTranslations = Kernel.Utils.JSON.valueToMaybe =<< questionTranslations,
                                                                                                       rating = rating,
                                                                                                       merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                       merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId}
instance ToTType' Beam.FeedbackForm Domain.Types.FeedbackForm.FeedbackForm
    where toTType' (Domain.Types.FeedbackForm.FeedbackForm {..}) = do Beam.FeedbackFormT{Beam.answer = answer,
                                                                                         Beam.answerType = answerType,
                                                                                         Beam.badges = Data.Aeson.toJSON <$> badges,
                                                                                         Beam.categoryName = categoryName,
                                                                                         Beam.id = Kernel.Types.Id.getId id,
                                                                                         Beam.question = question,
                                                                                         Beam.questionTranslations = Data.Aeson.toJSON <$> questionTranslations,
                                                                                         Beam.rating = rating,
                                                                                         Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                         Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId}



