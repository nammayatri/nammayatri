{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.WalkLegMultimodal where

import qualified Domain.Types.WalkLegMultimodal
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.WalkLegMultimodal as Beam
import qualified Storage.Queries.Transformers.MultiModal

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.WalkLegMultimodal.WalkLegMultimodal -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.WalkLegMultimodal.WalkLegMultimodal] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.WalkLegMultimodal.WalkLegMultimodal -> m (Maybe Domain.Types.WalkLegMultimodal.WalkLegMultimodal))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.WalkLegMultimodal.WalkLegMultimodal -> m (Maybe Domain.Types.WalkLegMultimodal.WalkLegMultimodal))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.WalkLegMultimodal.WalkLegMultimodal -> m ())
updateByPrimaryKey (Domain.Types.WalkLegMultimodal.WalkLegMultimodal {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.distanceUnit ((.unit) estimatedDistance),
      Se.Set Beam.estimatedDistance ((.value) estimatedDistance),
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.fromLocationId (Just $ Kernel.Types.Id.getId ((.id) fromLocation)),
      Se.Set Beam.agency (journeyLegInfo >>= (.agency)),
      Se.Set Beam.convenienceCost (Kernel.Prelude.fmap (.convenienceCost) journeyLegInfo),
      Se.Set Beam.journeyId (Kernel.Prelude.fmap (.journeyId) journeyLegInfo),
      Se.Set Beam.journeyLegOrder (Kernel.Prelude.fmap (.journeyLegOrder) journeyLegInfo),
      Se.Set Beam.skipBooking (Kernel.Prelude.fmap (.skipBooking) journeyLegInfo),
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.startTime startTime,
      Se.Set Beam.toLocationId (Kernel.Types.Id.getId <$> (toLocation <&> (.id))),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.WalkLegMultimodal Domain.Types.WalkLegMultimodal.WalkLegMultimodal where
  fromTType' (Beam.WalkLegMultimodalT {..}) = do
    fromLocation' <- Storage.Queries.Transformers.MultiModal.getFromLocation id
    toLocation' <- Storage.Queries.Transformers.MultiModal.getToLocation id
    pure $
      Just
        Domain.Types.WalkLegMultimodal.WalkLegMultimodal
          { estimatedDistance = Kernel.Types.Common.Distance estimatedDistance distanceUnit,
            estimatedDuration = estimatedDuration,
            fromLocation = fromLocation',
            id = Kernel.Types.Id.Id id,
            journeyLegInfo = Storage.Queries.Transformers.MultiModal.mkJourneyLegInfo agency convenienceCost journeyId journeyLegOrder skipBooking,
            riderId = Kernel.Types.Id.Id riderId,
            startTime = startTime,
            toLocation = toLocation',
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.WalkLegMultimodal Domain.Types.WalkLegMultimodal.WalkLegMultimodal where
  toTType' (Domain.Types.WalkLegMultimodal.WalkLegMultimodal {..}) = do
    Beam.WalkLegMultimodalT
      { Beam.distanceUnit = (.unit) estimatedDistance,
        Beam.estimatedDistance = (.value) estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.fromLocationId = Just $ Kernel.Types.Id.getId ((.id) fromLocation),
        Beam.id = Kernel.Types.Id.getId id,
        Beam.agency = journeyLegInfo >>= (.agency),
        Beam.convenienceCost = Kernel.Prelude.fmap (.convenienceCost) journeyLegInfo,
        Beam.journeyId = Kernel.Prelude.fmap (.journeyId) journeyLegInfo,
        Beam.journeyLegOrder = Kernel.Prelude.fmap (.journeyLegOrder) journeyLegInfo,
        Beam.skipBooking = Kernel.Prelude.fmap (.skipBooking) journeyLegInfo,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.startTime = startTime,
        Beam.toLocationId = Kernel.Types.Id.getId <$> (toLocation <&> (.id)),
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
