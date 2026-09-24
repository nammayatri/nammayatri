{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FareParametersAmbulanceDetails where

import qualified Domain.Types.FareParametersAmbulanceDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FareParametersAmbulanceDetails as Beam

instance FromTType' Beam.FareParametersAmbulanceDetails Domain.Types.FareParametersAmbulanceDetails.FareParametersAmbulanceDetails where
  fromTType' (Beam.FareParametersAmbulanceDetailsT {..}) = do
    pure $
      Just
        Domain.Types.FareParametersAmbulanceDetails.FareParametersAmbulanceDetails
          { cgst = cgst,
            currency = currency,
            distBasedFare = distBasedFare,
            fareParametersId = fareParametersId,
            platformFee = platformFee,
            sgst = sgst
          }

instance ToTType' Beam.FareParametersAmbulanceDetails Domain.Types.FareParametersAmbulanceDetails.FareParametersAmbulanceDetails where
  toTType' (Domain.Types.FareParametersAmbulanceDetails.FareParametersAmbulanceDetails {..}) = do
    Beam.FareParametersAmbulanceDetailsT
      { Beam.cgst = cgst,
        Beam.currency = currency,
        Beam.distBasedFare = distBasedFare,
        Beam.fareParametersId = fareParametersId,
        Beam.platformFee = platformFee,
        Beam.sgst = sgst
      }
