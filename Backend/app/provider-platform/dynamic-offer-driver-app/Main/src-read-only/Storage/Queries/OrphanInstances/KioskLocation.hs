{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.KioskLocation where

import qualified Domain.Types.KioskLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.KioskLocation as Beam

instance FromTType' Beam.KioskLocation Domain.Types.KioskLocation.KioskLocation where
  fromTType' (Beam.KioskLocationT {..}) = do
    pure $
      Just
        Domain.Types.KioskLocation.KioskLocation
          { address = address,
            contact = contact,
            id = Kernel.Types.Id.Id id,
            landmark = landmark,
            latitude = latitude,
            longitude = longitude,
            merchantId = Kernel.Types.Id.Id merchantId
          }

instance ToTType' Beam.KioskLocation Domain.Types.KioskLocation.KioskLocation where
  toTType' (Domain.Types.KioskLocation.KioskLocation {..}) = do
    Beam.KioskLocationT
      { Beam.address = address,
        Beam.contact = contact,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.landmark = landmark,
        Beam.latitude = latitude,
        Beam.longitude = longitude,
        Beam.merchantId = Kernel.Types.Id.getId merchantId
      }
