{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.VehicleDetails where

import qualified Domain.Types.VehicleDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.VehicleDetails as Beam

instance FromTType' Beam.VehicleDetails Domain.Types.VehicleDetails.VehicleDetails where
  fromTType' (Beam.VehicleDetailsT {..}) = do
    pure $
      Just
        Domain.Types.VehicleDetails.VehicleDetails
          { acAvailable = acAvailable,
            capacity = capacity,
            id = Kernel.Types.Id.Id id,
            make = make,
            model = model,
            vehicleVariant = vehicleVariant,
            year = year
          }

instance ToTType' Beam.VehicleDetails Domain.Types.VehicleDetails.VehicleDetails where
  toTType' (Domain.Types.VehicleDetails.VehicleDetails {..}) = do
    Beam.VehicleDetailsT
      { Beam.acAvailable = acAvailable,
        Beam.capacity = capacity,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.make = make,
        Beam.model = model,
        Beam.vehicleVariant = vehicleVariant,
        Beam.year = year
      }
