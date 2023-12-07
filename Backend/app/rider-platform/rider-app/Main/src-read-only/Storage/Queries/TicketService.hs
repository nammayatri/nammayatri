{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketService where

import qualified Domain.Types.BusinessHour
import qualified Domain.Types.TicketService
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketService as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketService.TicketService -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.TicketService.TicketService] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> m (Maybe (Domain.Types.TicketService.TicketService))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

getTicketServicesByPlaceId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Text -> m ([Domain.Types.TicketService.TicketService])
getTicketServicesByPlaceId placesId = do
  findAllWithKV
    [ Se.Is Beam.placesId $ Se.Eq placesId
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> m (Maybe (Domain.Types.TicketService.TicketService))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Bool -> [Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour] -> Domain.Types.TicketService.ExpiryType -> Kernel.Prelude.Int -> [Kernel.Prelude.Text] -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> m ()
updateByPrimaryKey allowFutureBooking businessHours expiry maxVerification operationalDays placesId service shortDesc (Kernel.Types.Id.Id id) = do
  updateWithKV
    [ Se.Set Beam.allowFutureBooking allowFutureBooking,
      Se.Set Beam.businessHours (Kernel.Types.Id.getId <$> businessHours),
      Se.Set Beam.expiry expiry,
      Se.Set Beam.maxVerification maxVerification,
      Se.Set Beam.operationalDays operationalDays,
      Se.Set Beam.placesId placesId,
      Se.Set Beam.service service,
      Se.Set Beam.shortDesc shortDesc
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

instance FromTType' Beam.TicketService Domain.Types.TicketService.TicketService where
  fromTType' Beam.TicketServiceT {..} = do
    pure $
      Just
        Domain.Types.TicketService.TicketService
          { allowFutureBooking = allowFutureBooking,
            businessHours = Kernel.Types.Id.Id <$> businessHours,
            expiry = expiry,
            id = Kernel.Types.Id.Id id,
            maxVerification = maxVerification,
            operationalDays = operationalDays,
            placesId = placesId,
            service = service,
            shortDesc = shortDesc
          }

instance ToTType' Beam.TicketService Domain.Types.TicketService.TicketService where
  toTType' Domain.Types.TicketService.TicketService {..} = do
    Beam.TicketServiceT
      { Beam.allowFutureBooking = allowFutureBooking,
        Beam.businessHours = Kernel.Types.Id.getId <$> businessHours,
        Beam.expiry = expiry,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxVerification = maxVerification,
        Beam.operationalDays = operationalDays,
        Beam.placesId = placesId,
        Beam.service = service,
        Beam.shortDesc = shortDesc
      }
