{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.Booking (module Storage.Queries.Booking, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.BookingExtra as ReExport
import Storage.Queries.Transformers.Booking
import qualified Domain.Types.Booking
import qualified Storage.Beam.Booking as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.Quote
import qualified Sequelize as Se



findByDashboardAgentId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.Booking.Booking]))
findByDashboardAgentId limit offset dashboardAgentId = do findAllWithOptionsKV [Se.Is Beam.dashboardAgentId $ Se.Eq dashboardAgentId] (Se.Desc Beam.createdAt) limit offset
findByQuoteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Quote.Quote) -> m (Maybe Domain.Types.Booking.Booking))
findByQuoteId quoteId = do findOneWithKV [Se.Is Beam.quoteId $ Se.Eq (Kernel.Types.Id.getId <$> quoteId)]
updateIsBookingUpdated :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m ())
updateIsBookingUpdated isBookingUpdated id = do {_now <- getCurrentTime;
                                                 updateWithKV [Se.Set Beam.isBookingUpdated ((Just isBookingUpdated)), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe Domain.Types.Booking.Booking))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]



