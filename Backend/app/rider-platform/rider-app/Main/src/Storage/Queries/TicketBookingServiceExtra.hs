module Storage.Queries.TicketBookingServiceExtra where

import qualified Data.Time.Calendar
import qualified Domain.Types.BusinessHour as DTBH
import qualified Domain.Types.TicketBookingService as DTBS
import qualified Domain.Types.TicketService as DTS
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Sequelize as Se
import qualified Storage.Beam.TicketBookingService as BeamR
import Storage.Queries.OrphanInstances.TicketBookingService ()

-- Extra code goes here --

findByIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id.Id DTBS.TicketBookingService] ->
  m [DTBS.TicketBookingService]
findByIds ticketBookingServiceIds = do
  findAllWithKV [Se.Is BeamR.id $ Se.In $ Id.getId <$> ticketBookingServiceIds]

findByShortIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id.ShortId DTBS.TicketBookingService] ->
  m [DTBS.TicketBookingService]
findByShortIds ticketBookingServiceShortIds = do
  findAllWithKV [Se.Is BeamR.shortId $ Se.In $ Id.getShortId <$> ticketBookingServiceShortIds]

findByVisitDateAndStatusAndServiceIdAndBtype ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Data.Time.Calendar.Day -> DTBS.ServiceStatus -> Id.Id DTS.TicketService -> DTBH.BusinessHourType -> Maybe Kernel.Prelude.UTCTime -> m [DTBS.TicketBookingService])
findByVisitDateAndStatusAndServiceIdAndBtype visitDate status (Id.Id ticketServiceId) btype expiryDate = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamR.visitDate $ Se.Eq visitDate,
          Se.Is BeamR.status $ Se.Eq status,
          Se.Is BeamR.ticketServiceId $ Se.Eq ticketServiceId,
          Se.Is BeamR.btype $ Se.Eq btype,
          Se.Is BeamR.expiryDate $ Se.GreaterThanOrEq expiryDate
        ]
    ]

-- | TimeWindow rows (not extensions) of a service that may hold capacity, by start date.
findTimeWindowCandidatesByServiceId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id.Id DTS.TicketService ->
  [Data.Time.Calendar.Day] ->
  m [DTBS.TicketBookingService]
-- Null-checks are filtered here, not in the query: KV cannot look up a secondary key by Nothing.
findTimeWindowCandidatesByServiceId (Id.Id ticketServiceId) visitDates =
  filter (\r -> isNothing r.parentBookingServiceId && isJust r.startTime)
    <$> findAllWithKV
      [ Se.And
          [ Se.Is BeamR.visitDate $ Se.In (Just <$> visitDates),
            Se.Is BeamR.ticketServiceId $ Se.Eq ticketServiceId,
            Se.Is BeamR.status $ Se.In [DTBS.Pending, DTBS.Confirmed, DTBS.Verified]
          ]
      ]

-- | Paid TimeWindow rows (not extensions) for a plate, in any place.
findPaidTimeWindowByVehicleNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  m [DTBS.TicketBookingService]
findPaidTimeWindowByVehicleNumber vehicleNumber =
  filter (isNothing . (.parentBookingServiceId))
    <$> findAllWithKV
      [ Se.And
          [ Se.Is BeamR.vehicleNumber $ Se.Eq (Just vehicleNumber),
            Se.Is BeamR.status $ Se.In [DTBS.Confirmed, DTBS.Verified]
          ]
      ]
