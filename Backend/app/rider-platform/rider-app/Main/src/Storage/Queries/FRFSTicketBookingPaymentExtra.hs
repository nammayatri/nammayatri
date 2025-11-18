module Storage.Queries.FRFSTicketBookingPaymentExtra where

import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBookingPayment as Beam
import Storage.Queries.OrphanInstances.FRFSTicketBookingPayment ()

findNewTBPByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DFRFSTicketBooking.FRFSTicketBooking -> m (Maybe DFRFSTicketBookingPayment.FRFSTicketBookingPayment)
findNewTBPByBookingId (Id bookingId) =
  findAllWithOptionsKV [Se.Is Beam.frfsTicketBookingId $ Se.Eq bookingId] (Se.Desc Beam.createdAt) (Just 1) Nothing <&> listToMaybe

findAllTBPByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DFRFSTicketBooking.FRFSTicketBooking -> m [DFRFSTicketBookingPayment.FRFSTicketBookingPayment]
findAllTBPByBookingId (Id bookingId) =
  findAllWithOptionsKV [Se.Is Beam.frfsTicketBookingId $ Se.Eq bookingId] (Se.Desc Beam.createdAt) Nothing Nothing
