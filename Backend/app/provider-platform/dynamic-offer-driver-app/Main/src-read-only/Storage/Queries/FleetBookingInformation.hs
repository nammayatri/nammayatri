{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.FleetBookingInformation (module Storage.Queries.FleetBookingInformation, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.FleetBookingInformationExtra as ReExport
import qualified Domain.Types.FleetBookingInformation
import qualified Storage.Beam.FleetBookingInformation as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetBookingInformation.FleetBookingInformation -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetBookingInformation.FleetBookingInformation] -> m ())
createMany = traverse_ create
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
            (Kernel.Types.Id.Id Domain.Types.FleetBookingInformation.FleetBookingInformation -> m (Maybe Domain.Types.FleetBookingInformation.FleetBookingInformation))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findByServiceId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.FleetBookingInformation.FleetBookingInformation))
findByServiceId serviceId = do findOneWithKV [Se.Is Beam.serviceId $ Se.Eq serviceId]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.FleetBookingInformation.FleetBookingInformation -> m (Maybe Domain.Types.FleetBookingInformation.FleetBookingInformation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetBookingInformation.FleetBookingInformation -> m ())
updateByPrimaryKey (Domain.Types.FleetBookingInformation.FleetBookingInformation {..}) = do {_now <- getCurrentTime;
                                                                                             updateWithKV [Se.Set Beam.amount amount,
                                                                                                           Se.Set Beam.bookedSeats bookedSeats,
                                                                                                           Se.Set Beam.bookingId bookingId,
                                                                                                           Se.Set Beam.customerMobileNumberEncrypted (((customerMobileNumber <&> unEncrypted . (.encrypted)))),
                                                                                                           Se.Set Beam.customerMobileNumberHash ((customerMobileNumber <&> (.hash))),
                                                                                                           Se.Set Beam.customerName customerName,
                                                                                                           Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId <$> fleetOwnerId),
                                                                                                           Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                                           Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
                                                                                                           Se.Set Beam.paymentMethod paymentMethod,
                                                                                                           Se.Set Beam.personId (Kernel.Types.Id.getId <$> personId),
                                                                                                           Se.Set Beam.placeName placeName,
                                                                                                           Se.Set Beam.serviceId serviceId,
                                                                                                           Se.Set Beam.serviceName serviceName,
                                                                                                           Se.Set Beam.status status,
                                                                                                           Se.Set Beam.ticketBookingServiceShortId ticketBookingServiceShortId,
                                                                                                           Se.Set Beam.ticketBookingShortId ticketBookingShortId,
                                                                                                           Se.Set Beam.ticketPlaceId ticketPlaceId,
                                                                                                           Se.Set Beam.updatedAt _now,
                                                                                                           Se.Set Beam.vehicleNo vehicleNo,
                                                                                                           Se.Set Beam.visitDate visitDate] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



