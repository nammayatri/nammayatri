{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverOffer where

import qualified Domain.Types.DriverOffer
import qualified Domain.Types.Estimate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOffer as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverOffer.DriverOffer -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverOffer.DriverOffer] -> m ())
createMany = traverse_ create

findByEstimateIdAndBppQuoteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Kernel.Prelude.Text -> m [Domain.Types.DriverOffer.DriverOffer])
findByEstimateIdAndBppQuoteId estimateId bppQuoteId = do findAllWithKV [Se.And [Se.Is Beam.estimateId $ Se.Eq (Kernel.Types.Id.getId estimateId), Se.Is Beam.bppQuoteId $ Se.Eq bppQuoteId]]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverOffer.DriverOffer -> m (Maybe Domain.Types.DriverOffer.DriverOffer))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverOffer.DriverOfferStatus -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m ())
updateStatus status estimateId = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.updatedAt _now, Se.Set Beam.status status] [Se.Is Beam.estimateId $ Se.Eq (Kernel.Types.Id.getId estimateId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverOffer.DriverOffer -> m (Maybe Domain.Types.DriverOffer.DriverOffer))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverOffer.DriverOffer -> m ())
updateByPrimaryKey (Domain.Types.DriverOffer.DriverOffer {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppQuoteId bppQuoteId,
      Se.Set Beam.createdAt (Kernel.Prelude.Just createdAt),
      Se.Set Beam.distanceToPickup (Kernel.Types.Common.distanceToHighPrecMeters <$> distanceToPickup),
      Se.Set Beam.distanceToPickupValue (Kernel.Types.Common.distanceToHighPrecDistance distanceUnit <$> distanceToPickup),
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.driverName driverName,
      Se.Set Beam.durationToPickup durationToPickup,
      Se.Set Beam.estimateId (Kernel.Types.Id.getId estimateId),
      Se.Set Beam.fulfillmentId fulfillmentId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.rating rating,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverOffer Domain.Types.DriverOffer.DriverOffer where
  fromTType' (Beam.DriverOfferT {..}) = do
    pure $
      Just
        Domain.Types.DriverOffer.DriverOffer
          { bppQuoteId = bppQuoteId,
            createdAt = Kernel.Prelude.fromMaybe updatedAt createdAt,
            distanceToPickup = Kernel.Types.Common.mkDistanceWithDefault distanceUnit distanceToPickupValue <$> distanceToPickup,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverName = driverName,
            durationToPickup = durationToPickup,
            estimateId = Kernel.Types.Id.Id estimateId,
            fulfillmentId = fulfillmentId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            rating = rating,
            status = status,
            updatedAt = updatedAt,
            validTill = validTill
          }

instance ToTType' Beam.DriverOffer Domain.Types.DriverOffer.DriverOffer where
  toTType' (Domain.Types.DriverOffer.DriverOffer {..}) = do
    Beam.DriverOfferT
      { Beam.bppQuoteId = bppQuoteId,
        Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.distanceToPickup = Kernel.Types.Common.distanceToHighPrecMeters <$> distanceToPickup,
        Beam.distanceToPickupValue = Kernel.Types.Common.distanceToHighPrecDistance distanceUnit <$> distanceToPickup,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverName = driverName,
        Beam.durationToPickup = durationToPickup,
        Beam.estimateId = Kernel.Types.Id.getId estimateId,
        Beam.fulfillmentId = fulfillmentId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.rating = rating,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill
      }
