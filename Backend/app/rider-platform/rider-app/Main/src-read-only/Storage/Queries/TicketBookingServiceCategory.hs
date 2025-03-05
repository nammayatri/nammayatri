{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketBookingServiceCategory (module Storage.Queries.TicketBookingServiceCategory, module ReExport) where

import qualified Data.Aeson
import qualified Data.Time
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.TicketBookingService
import qualified Domain.Types.TicketBookingServiceCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketBookingServiceCategory as Beam
import Storage.Queries.TicketBookingServiceCategoryExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory] -> m ())
createMany = traverse_ create

findAllByServiceCategoryIdDateAndBtype ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Domain.Types.BusinessHour.BusinessHourType -> m [Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory])
findAllByServiceCategoryIdDateAndBtype serviceCategoryId visitDate btype = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.serviceCategoryId $ Se.Eq serviceCategoryId,
          Se.Is Beam.visitDate $ Se.Eq visitDate,
          Se.Is Beam.btype $ Se.Eq btype
        ]
    ]

findAllByTicketBookingServiceId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m [Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory])
findAllByTicketBookingServiceId ticketBookingServiceId = do findAllWithKV [Se.Is Beam.ticketBookingServiceId $ Se.Eq (Kernel.Types.Id.getId ticketBookingServiceId)]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory -> m (Maybe Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateCancellationDetailsById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Domain.Types.TicketBookingServiceCategory.CancelledBy -> Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory -> m ())
updateCancellationDetailsById cancelledSeats amountToRefund eventCancelledBy id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cancelledSeats cancelledSeats,
      Se.Set Beam.amountToRefund amountToRefund,
      Se.Set Beam.eventCancelledBy eventCancelledBy,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory -> m (Maybe Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory -> m ())
updateByPrimaryKey (Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount ((.amount) amount),
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) amount),
      Se.Set Beam.amountToRefund amountToRefund,
      Se.Set Beam.bookedSeats bookedSeats,
      Se.Set Beam.btype btype,
      Se.Set Beam.cancelledSeats cancelledSeats,
      Se.Set Beam.eventCancelledBy eventCancelledBy,
      Se.Set Beam.name name,
      Se.Set Beam.serviceCategoryId serviceCategoryId,
      Se.Set Beam.ticketBookingServiceId (Kernel.Types.Id.getId ticketBookingServiceId),
      Se.Set Beam.vendorSplitDetails (Data.Aeson.toJSON <$> vendorSplitDetails),
      Se.Set Beam.visitDate visitDate,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
