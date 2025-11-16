{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketSubPlace where

import qualified Data.Aeson
import qualified Domain.Types.TicketPlace
import qualified Domain.Types.TicketSubPlace
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketSubPlace as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketSubPlace.TicketSubPlace -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TicketSubPlace.TicketSubPlace] -> m ())
createMany = traverse_ create

findActiveByTicketPlaceId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Bool -> m [Domain.Types.TicketSubPlace.TicketSubPlace])
findActiveByTicketPlaceId ticketPlaceId isActive = do findAllWithKV [Se.And [Se.Is Beam.ticketPlaceId $ Se.Eq (Kernel.Types.Id.getId ticketPlaceId), Se.Is Beam.isActive $ Se.Eq isActive]]

findAllByTicketPlaceId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m [Domain.Types.TicketSubPlace.TicketSubPlace])
findAllByTicketPlaceId ticketPlaceId = do findAllWithKV [Se.Is Beam.ticketPlaceId $ Se.Eq (Kernel.Types.Id.getId ticketPlaceId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace -> m (Maybe Domain.Types.TicketSubPlace.TicketSubPlace))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace -> m (Maybe Domain.Types.TicketSubPlace.TicketSubPlace))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketSubPlace.TicketSubPlace -> m ())
updateByPrimaryKey (Domain.Types.TicketSubPlace.TicketSubPlace {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.enforcedTicketPlaceId (Kernel.Types.Id.getId <$> enforcedTicketPlaceId),
      Se.Set Beam.isActive isActive,
      Se.Set Beam.name name,
      Se.Set Beam.rules (Data.Aeson.toJSON <$> rules),
      Se.Set Beam.subPlaceType subPlaceType,
      Se.Set Beam.ticketPlaceId (Kernel.Types.Id.getId ticketPlaceId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TicketSubPlace Domain.Types.TicketSubPlace.TicketSubPlace where
  fromTType' (Beam.TicketSubPlaceT {..}) = do
    pure $
      Just
        Domain.Types.TicketSubPlace.TicketSubPlace
          { createdAt = createdAt,
            description = description,
            enforcedTicketPlaceId = Kernel.Types.Id.Id <$> enforcedTicketPlaceId,
            id = Kernel.Types.Id.Id id,
            isActive = isActive,
            name = name,
            rules = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< rules,
            subPlaceType = subPlaceType,
            ticketPlaceId = Kernel.Types.Id.Id ticketPlaceId,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.TicketSubPlace Domain.Types.TicketSubPlace.TicketSubPlace where
  toTType' (Domain.Types.TicketSubPlace.TicketSubPlace {..}) = do
    Beam.TicketSubPlaceT
      { Beam.createdAt = createdAt,
        Beam.description = description,
        Beam.enforcedTicketPlaceId = Kernel.Types.Id.getId <$> enforcedTicketPlaceId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isActive = isActive,
        Beam.name = name,
        Beam.rules = Data.Aeson.toJSON <$> rules,
        Beam.subPlaceType = subPlaceType,
        Beam.ticketPlaceId = Kernel.Types.Id.getId ticketPlaceId,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
