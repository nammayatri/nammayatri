{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RideFeedbackResponse where

import qualified Data.Aeson
import qualified Data.Text
import qualified Domain.Types.RideFeedbackResponse
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Storage.Beam.RideFeedbackResponse as Beam

instance FromTType' Beam.RideFeedbackResponse Domain.Types.RideFeedbackResponse.RideFeedbackResponse where
  fromTType' (Beam.RideFeedbackResponseT {..}) = do
    pure $
      Just
        Domain.Types.RideFeedbackResponse.RideFeedbackResponse
          { actionResults = Kernel.Utils.JSON.valueToMaybe =<< actionResults,
            answer = Kernel.Utils.JSON.valueToMaybe =<< answer,
            bookingId = Kernel.Types.Id.Id bookingId,
            configId = Kernel.Types.Id.Id configId,
            configVersion = configVersion,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            logicVersion = logicVersion,
            lon = lon,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            parentResponseId = Kernel.Types.Id.Id <$> parentResponseId,
            personId = Kernel.Types.Id.Id personId,
            questionKey = questionKey,
            rideId = Kernel.Types.Id.Id rideId,
            rideStatusAtResponse = (Kernel.Prelude.readMaybe . Data.Text.unpack =<< rideStatusAtResponse),
            secondsIntoRide = secondsIntoRide,
            selectedOptionKeys = selectedOptionKeys,
            shownCount = shownCount,
            status = status,
            updatedAt = updatedAt,
            vehicleServiceTierType = vehicleServiceTierType,
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.RideFeedbackResponse Domain.Types.RideFeedbackResponse.RideFeedbackResponse where
  toTType' (Domain.Types.RideFeedbackResponse.RideFeedbackResponse {..}) = do
    Beam.RideFeedbackResponseT
      { Beam.actionResults = Data.Aeson.toJSON <$> actionResults,
        Beam.answer = Data.Aeson.toJSON <$> answer,
        Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.configId = Kernel.Types.Id.getId configId,
        Beam.configVersion = configVersion,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.logicVersion = logicVersion,
        Beam.lon = lon,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.parentResponseId = Kernel.Types.Id.getId <$> parentResponseId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.questionKey = questionKey,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.rideStatusAtResponse = Kernel.Prelude.show <$> rideStatusAtResponse,
        Beam.secondsIntoRide = secondsIntoRide,
        Beam.selectedOptionKeys = selectedOptionKeys,
        Beam.shownCount = shownCount,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.vehicleServiceTierType = vehicleServiceTierType,
        Beam.vehicleVariant = vehicleVariant
      }
