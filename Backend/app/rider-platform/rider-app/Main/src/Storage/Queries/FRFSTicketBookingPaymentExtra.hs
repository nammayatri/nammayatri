module Storage.Queries.FRFSTicketBookingPaymentExtra where

import Control.Lens ((^?), _head)
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBookingPayment as Beam
import Storage.Queries.OrphanInstances.FRFSTicketBookingPayment ()

findTicketBookingPayment :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DFRFSTicketBooking.FRFSTicketBooking -> m (Maybe DFRFSTicketBookingPayment.FRFSTicketBookingPayment)
findTicketBookingPayment booking =
  maybe (pure Nothing) (\paymentBookingId -> findOneWithKV [Se.Is Beam.id $ Se.Eq paymentBookingId]) booking.frfsTicketBookingPaymentIdForTicketGeneration
    |<|>| (findAllWithOptionsKV [Se.Is Beam.frfsTicketBookingId $ Se.Eq booking.id.getId] (Se.Desc Beam.createdAt) (Just 1) Nothing <&> (^? _head))

findAllTBPByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DFRFSTicketBooking.FRFSTicketBooking -> m [DFRFSTicketBookingPayment.FRFSTicketBookingPayment]
findAllTBPByBookingId (Id bookingId) =
  findAllWithOptionsKV [Se.Is Beam.frfsTicketBookingId $ Se.Eq bookingId] (Se.Desc Beam.createdAt) Nothing Nothing
