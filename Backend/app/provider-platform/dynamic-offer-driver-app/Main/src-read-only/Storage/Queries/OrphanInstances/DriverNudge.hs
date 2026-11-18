{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverNudge where

import qualified Data.Aeson
import qualified Domain.Types.DriverNudge
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverNudge as Beam

instance FromTType' Beam.DriverNudge Domain.Types.DriverNudge.DriverNudge where
  fromTType' (Beam.DriverNudgeT {..}) = do
    pure $
      Just
        Domain.Types.DriverNudge.DriverNudge
          { actualDistance = actualDistance,
            createdAt = createdAt,
            destination = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< destination,
            driverId = Kernel.Types.Id.Id driverId,
            estimatedDistance = estimatedDistance,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverNudge Domain.Types.DriverNudge.DriverNudge where
  toTType' (Domain.Types.DriverNudge.DriverNudge {..}) = do
    Beam.DriverNudgeT
      { Beam.actualDistance = actualDistance,
        Beam.createdAt = createdAt,
        Beam.destination = Kernel.Prelude.toJSON <$> destination,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.estimatedDistance = estimatedDistance,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
