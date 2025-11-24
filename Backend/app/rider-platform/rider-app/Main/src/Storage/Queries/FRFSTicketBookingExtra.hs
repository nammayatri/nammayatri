module Storage.Queries.FRFSTicketBookingExtra where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBooking as Beam
import Storage.Queries.OrphanInstances.FRFSTicketBooking ()

-- Extra code goes here --

updateBookingAuthCodeById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Text -> Id FRFSTicketBooking -> m ()
updateBookingAuthCodeById bookingAuthCode id = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.bookingAuthCode bookingAuthCode, Se.Set Beam.updatedAt now] [Se.Is Beam.id $ Se.Eq (id.getId)]

insertPayerVpaIfNotPresent :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Text -> Id FRFSTicketBooking -> m ()
insertPayerVpaIfNotPresent (Just vpa) bookingId = do
  mbBooking <- findOneWithKV [Se.Is Beam.id $ Se.Eq (bookingId.getId)]
  whenJust mbBooking $ \booking -> do
    when (isNothing booking.payerVpa) $ do
      updatePayerVpaByBookingId bookingId vpa
  pure ()
insertPayerVpaIfNotPresent _ _ = pure ()

updatePayerVpaByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id FRFSTicketBooking -> Text -> m ()
updatePayerVpaByBookingId id payerVpa = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.payerVpa (Just payerVpa), Se.Set Beam.updatedAt now] [Se.Is Beam.id $ Se.Eq (id.getId)]

findAllByCashbackStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Int -> Maybe Int -> Maybe CashbackStatus -> m [FRFSTicketBooking]
findAllByCashbackStatus limit offset cashbackStatus = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.cashbackStatus $ Se.Eq cashbackStatus,
          Se.Is Beam.payerVpa $ Se.Not $ Se.Eq Nothing
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findAllByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Int -> Maybe Int -> Id Person -> Maybe Spec.VehicleCategory -> m [FRFSTicketBooking]
findAllByRiderId limit offset riderId mbVehicleCategory = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]
            <> [Se.Is Beam.vehicleType $ Se.Eq (fromJust mbVehicleCategory) | isJust mbVehicleCategory]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findAllByProviderNameAndCreatedAtAfterAndStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> UTCTime -> DFRFSTicketBookingStatus.FRFSTicketBookingStatus -> m [FRFSTicketBooking]
findAllByProviderNameAndCreatedAtAfterAndStatus providerName createdAtAfter status = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.providerName $ Se.Eq providerName,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq createdAtAfter,
          Se.Is Beam.status $ Se.Eq status
        ]
    ]
