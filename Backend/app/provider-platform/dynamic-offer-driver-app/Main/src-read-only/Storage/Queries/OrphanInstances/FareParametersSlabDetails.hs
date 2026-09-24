{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FareParametersSlabDetails where

import qualified Domain.Types.FareParametersSlabDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FareParametersSlabDetails as Beam

instance FromTType' Beam.FareParametersSlabDetails Domain.Types.FareParametersSlabDetails.FareParametersSlabDetails where
  fromTType' (Beam.FareParametersSlabDetailsT {..}) = do
    pure $
      Just
        Domain.Types.FareParametersSlabDetails.FareParametersSlabDetails
          { cgst = cgst,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            fareParametersId = fareParametersId,
            platformFee = platformFee,
            sgst = sgst
          }

instance ToTType' Beam.FareParametersSlabDetails Domain.Types.FareParametersSlabDetails.FareParametersSlabDetails where
  toTType' (Domain.Types.FareParametersSlabDetails.FareParametersSlabDetails {..}) = do
    Beam.FareParametersSlabDetailsT
      { Beam.cgst = cgst,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.fareParametersId = fareParametersId,
        Beam.platformFee = platformFee,
        Beam.sgst = sgst
      }
