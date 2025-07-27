{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.WalkLegMultimodal where

import qualified Domain.Types.WalkLegMultimodal
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.WalkLegMultimodal as Beam
import qualified Storage.Queries.Transformers.MultiModal

instance FromTType' Beam.WalkLegMultimodal Domain.Types.WalkLegMultimodal.WalkLegMultimodal where
  fromTType' (Beam.WalkLegMultimodalT {..}) = do
    fromLocation' <- Storage.Queries.Transformers.MultiModal.getFromLocation id
    toLocation' <- Storage.Queries.Transformers.MultiModal.getToLocation id
    pure $
      Just
        Domain.Types.WalkLegMultimodal.WalkLegMultimodal
          { estimatedDistance = Kernel.Types.Common.Distance <$> estimatedDistance <*> distanceUnit,
            estimatedDuration = estimatedDuration,
            fromLocation = fromLocation',
            id = Kernel.Types.Id.Id id,
            journeyLegInfo = Storage.Queries.Transformers.MultiModal.mkJourneyLegInfo agency convenienceCost isDeleted journeyId journeyLegOrder onSearchFailed pricingId skipBooking,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            riderId = Kernel.Types.Id.Id riderId,
            startTime = startTime,
            status = status,
            toLocation = toLocation',
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.WalkLegMultimodal Domain.Types.WalkLegMultimodal.WalkLegMultimodal where
  toTType' (Domain.Types.WalkLegMultimodal.WalkLegMultimodal {..}) = do
    Beam.WalkLegMultimodalT
      { Beam.distanceUnit = (.unit) <$> estimatedDistance,
        Beam.estimatedDistance = (.value) <$> estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.fromLocationId = Just $ Kernel.Types.Id.getId ((.id) fromLocation),
        Beam.id = Kernel.Types.Id.getId id,
        Beam.agency = journeyLegInfo >>= (.agency),
        Beam.convenienceCost = Kernel.Prelude.fmap (.convenienceCost) journeyLegInfo,
        Beam.isDeleted = journeyLegInfo >>= (.isDeleted),
        Beam.journeyId = Kernel.Prelude.fmap (.journeyId) journeyLegInfo,
        Beam.journeyLegOrder = Kernel.Prelude.fmap (.journeyLegOrder) journeyLegInfo,
        Beam.onSearchFailed = journeyLegInfo >>= (.onSearchFailed),
        Beam.pricingId = journeyLegInfo >>= (.pricingId),
        Beam.skipBooking = Kernel.Prelude.fmap (.skipBooking) journeyLegInfo,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.startTime = startTime,
        Beam.status = status,
        Beam.toLocationId = Kernel.Types.Id.getId <$> (toLocation <&> (.id)),
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
