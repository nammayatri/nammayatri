{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketService where

import qualified Domain.Types.BusinessHour as Domain.Types.BusinessHour
import qualified Domain.Types.TicketService as Domain.Types.TicketService
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude as Kernel.Prelude
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketService as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketService.TicketService -> m ()
create = createWithKV

getTicketServicesByPlaceId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Text -> m ([Domain.Types.TicketService.TicketService])
getTicketServicesByPlaceId placeId = do
  findAllWithKV
    [ Se.Is Beam.placeId $ Se.Eq placeId
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
            placeId = placeId,
            serviceName = serviceName,
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
        Beam.placeId = placeId,
        Beam.serviceName = serviceName,
        Beam.shortDesc = shortDesc
      }
