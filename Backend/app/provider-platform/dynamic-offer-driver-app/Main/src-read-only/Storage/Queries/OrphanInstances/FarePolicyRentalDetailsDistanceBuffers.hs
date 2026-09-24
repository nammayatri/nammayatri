{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FarePolicyRentalDetailsDistanceBuffers where

import qualified Domain.Types.FarePolicyRentalDetailsDistanceBuffers
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FarePolicyRentalDetailsDistanceBuffers as Beam

instance FromTType' Beam.FarePolicyRentalDetailsDistanceBuffers Domain.Types.FarePolicyRentalDetailsDistanceBuffers.FarePolicyRentalDetailsDistanceBuffers where
  fromTType' (Beam.FarePolicyRentalDetailsDistanceBuffersT {..}) = do
    pure $
      Just
        Domain.Types.FarePolicyRentalDetailsDistanceBuffers.FarePolicyRentalDetailsDistanceBuffers
          { bufferKms = bufferKms,
            bufferMeters = bufferMeters,
            farePolicyId = farePolicyId,
            rideDuration = rideDuration
          }

instance ToTType' Beam.FarePolicyRentalDetailsDistanceBuffers Domain.Types.FarePolicyRentalDetailsDistanceBuffers.FarePolicyRentalDetailsDistanceBuffers where
  toTType' (Domain.Types.FarePolicyRentalDetailsDistanceBuffers.FarePolicyRentalDetailsDistanceBuffers {..}) = do
    Beam.FarePolicyRentalDetailsDistanceBuffersT
      { Beam.bufferKms = bufferKms,
        Beam.bufferMeters = bufferMeters,
        Beam.farePolicyId = farePolicyId,
        Beam.rideDuration = rideDuration
      }
