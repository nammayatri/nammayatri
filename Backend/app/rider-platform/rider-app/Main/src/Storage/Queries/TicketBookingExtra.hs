module Storage.Queries.TicketBookingExtra where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketBooking as DTB
import qualified Domain.Types.TicketPlace as DTP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Sequelize as Se
import Storage.Beam.TicketBooking as BeamR
import Storage.Queries.OrphanInstances.TicketBooking ()

-- Extra code goes here --
findByIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id.Id DTB.TicketBooking] ->
  m [DTB.TicketBooking]
findByIds ticketBookingIds = do
  findAllWithKV [Se.Is BeamR.id $ Se.In $ getId <$> ticketBookingIds]

findAllTicketBookingList ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Int ->
  Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Id DTP.TicketPlace ->
  Maybe Text ->
  Maybe DTB.BookingStatus ->
  m [DTB.TicketBooking]
findAllTicketBookingList merchantOperatingCityId limit offset mbFrom mbTo ticketPlaceId mbShortId mbStatus = do
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Is BeamR.merchantOperatingCityId $ Se.Eq (Id.getId merchantOperatingCityId),
            Se.Is BeamR.ticketPlaceId $ Se.Eq (Id.getId ticketPlaceId)
          ]
            ++ maybe [] (\shortId -> [Se.Is BeamR.shortId $ Se.Eq shortId]) mbShortId
            ++ maybe [] (\status -> [Se.Is BeamR.status $ Se.Eq status]) mbStatus
            ++ maybe [] (\from -> [Se.Is BeamR.createdAt $ Se.GreaterThanOrEq from]) mbFrom
            ++ maybe [] (\to -> [Se.Is BeamR.createdAt $ Se.LessThanOrEq to]) mbTo
        )
    ]
    (Se.Asc BeamR.createdAt)
    (Just limit)
    (Just offset)
