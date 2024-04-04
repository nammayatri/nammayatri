{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Image where

import qualified Domain.Types.Image
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Image as Beam

instance FromTType' Beam.Image Domain.Types.Image.Image where
  fromTType' (Beam.ImageT {..}) = do
    pure $
      Just
        Domain.Types.Image.Image
          { failureReason = failureReason,
            id = Kernel.Types.Id.Id id,
            imageType = imageType,
            isValid = isValid,
            merchantId = Kernel.Types.Id.Id merchantId,
            personId = Kernel.Types.Id.Id personId,
            rcId = rcId,
            s3Path = s3Path,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Image Domain.Types.Image.Image where
  toTType' (Domain.Types.Image.Image {..}) = do
    Beam.ImageT
      { Beam.failureReason = failureReason,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.imageType = imageType,
        Beam.isValid = isValid,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.rcId = rcId,
        Beam.s3Path = s3Path,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
