{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketPlace where

import qualified Data.Aeson
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketPlace
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketPlace as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketPlace.TicketPlace -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TicketPlace.TicketPlace] -> m ())
createMany = traverse_ create

findAllByTicketMerchantIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.TicketPlace.PlaceStatus -> m [Domain.Types.TicketPlace.TicketPlace])
findAllByTicketMerchantIdAndStatus ticketMerchantId status = do findAllWithKV [Se.And [Se.Is Beam.ticketMerchantId $ Se.Eq ticketMerchantId, Se.Is Beam.status $ Se.Eq status]]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m (Maybe Domain.Types.TicketPlace.TicketPlace))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByNameAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.TicketPlace.TicketPlace))
findByNameAndCity name merchantOperatingCityId = do findOneWithKV [Se.And [Se.Is Beam.name $ Se.Eq name, Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

getAllTicketPlaces :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketPlace.PlaceStatus -> m [Domain.Types.TicketPlace.TicketPlace])
getAllTicketPlaces status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

getTicketPlaces :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.TicketPlace.TicketPlace])
getTicketPlaces merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

updateGalleryById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Prelude.Text] -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m ())
updateGalleryById gallery id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.gallery gallery, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m (Maybe Domain.Types.TicketPlace.TicketPlace))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketPlace.TicketPlace -> m ())
updateByPrimaryKey (Domain.Types.TicketPlace.TicketPlace {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowSameDayBooking allowSameDayBooking,
      Se.Set Beam.closeTimings closeTimings,
      Se.Set Beam.customTabs (Data.Aeson.toJSON <$> customTabs),
      Se.Set Beam.description description,
      Se.Set Beam.gallery gallery,
      Se.Set Beam.iconUrl iconUrl,
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.mapImageUrl mapImageUrl,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.openTimings openTimings,
      Se.Set Beam.placeType placeType,
      Se.Set Beam.priority (Kernel.Prelude.Just priority),
      Se.Set Beam.rules (Data.Aeson.toJSON <$> rules),
      Se.Set Beam.shortDesc shortDesc,
      Se.Set Beam.status status,
      Se.Set Beam.termsAndConditions termsAndConditions,
      Se.Set Beam.termsAndConditionsUrl termsAndConditionsUrl,
      Se.Set Beam.ticketMerchantId ticketMerchantId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TicketPlace Domain.Types.TicketPlace.TicketPlace where
  fromTType' (Beam.TicketPlaceT {..}) = do
    pure $
      Just
        Domain.Types.TicketPlace.TicketPlace
          { allowSameDayBooking = allowSameDayBooking,
            closeTimings = closeTimings,
            customTabs = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< customTabs,
            description = description,
            gallery = gallery,
            iconUrl = iconUrl,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            mapImageUrl = mapImageUrl,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            openTimings = openTimings,
            placeType = placeType,
            priority = Kernel.Prelude.fromMaybe 0 priority,
            rules = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< rules,
            shortDesc = shortDesc,
            status = status,
            termsAndConditions = termsAndConditions,
            termsAndConditionsUrl = termsAndConditionsUrl,
            ticketMerchantId = ticketMerchantId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketPlace Domain.Types.TicketPlace.TicketPlace where
  toTType' (Domain.Types.TicketPlace.TicketPlace {..}) = do
    Beam.TicketPlaceT
      { Beam.allowSameDayBooking = allowSameDayBooking,
        Beam.closeTimings = closeTimings,
        Beam.customTabs = Data.Aeson.toJSON <$> customTabs,
        Beam.description = description,
        Beam.gallery = gallery,
        Beam.iconUrl = iconUrl,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.mapImageUrl = mapImageUrl,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.openTimings = openTimings,
        Beam.placeType = placeType,
        Beam.priority = Kernel.Prelude.Just priority,
        Beam.rules = Data.Aeson.toJSON <$> rules,
        Beam.shortDesc = shortDesc,
        Beam.status = status,
        Beam.termsAndConditions = termsAndConditions,
        Beam.termsAndConditionsUrl = termsAndConditionsUrl,
        Beam.ticketMerchantId = ticketMerchantId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
