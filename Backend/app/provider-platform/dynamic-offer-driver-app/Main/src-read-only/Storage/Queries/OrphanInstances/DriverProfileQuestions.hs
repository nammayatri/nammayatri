{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverProfileQuestions where

import qualified Domain.Types.DriverProfileQuestions
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverProfileQuestions as Beam

instance FromTType' Beam.DriverProfileQuestions Domain.Types.DriverProfileQuestions.DriverProfileQuestions where
  fromTType' (Beam.DriverProfileQuestionsT {..}) = do
    pure $
      Just
        Domain.Types.DriverProfileQuestions.DriverProfileQuestions
          { aboutMe = aboutMe,
            aspirations = aspirations,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            drivingSince = drivingSince,
            hometown = hometown,
            imageIds = imageIds,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            pledges = pledges,
            updatedAt = updatedAt,
            vehicleTags = vehicleTags
          }

instance ToTType' Beam.DriverProfileQuestions Domain.Types.DriverProfileQuestions.DriverProfileQuestions where
  toTType' (Domain.Types.DriverProfileQuestions.DriverProfileQuestions {..}) = do
    Beam.DriverProfileQuestionsT
      { Beam.aboutMe = aboutMe,
        Beam.aspirations = aspirations,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.drivingSince = drivingSince,
        Beam.hometown = hometown,
        Beam.imageIds = imageIds,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.pledges = pledges,
        Beam.updatedAt = updatedAt,
        Beam.vehicleTags = vehicleTags
      }
