{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketService (module Storage.Queries.TicketService, module ReExport) where

import qualified Data.Aeson
import qualified Domain.Types.TicketService
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketService as Beam
import Storage.Queries.TicketServiceExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketService.TicketService -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TicketService.TicketService] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> m (Maybe Domain.Types.TicketService.TicketService))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPlacesIdAndService :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.TicketService.TicketService))
findByPlacesIdAndService placesId service = do findOneWithKV [Se.And [Se.Is Beam.placesId $ Se.Eq placesId, Se.Is Beam.service $ Se.Eq service]]

getTicketServicesByPlaceId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.TicketService.TicketService])
getTicketServicesByPlaceId placesId = do findAllWithKV [Se.Is Beam.placesId $ Se.Eq placesId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> m (Maybe Domain.Types.TicketService.TicketService))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketService.TicketService -> m ())
updateByPrimaryKey (Domain.Types.TicketService.TicketService {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowCancellation allowCancellation,
      Se.Set Beam.allowFutureBooking allowFutureBooking,
      Se.Set Beam.businessHours (Kernel.Types.Id.getId <$> businessHours),
      Se.Set Beam.expiry expiry,
      Se.Set Beam.isClosed (Kernel.Prelude.Just isClosed),
      Se.Set Beam.maxVerification maxVerification,
      Se.Set Beam.operationalEndDate (operationalDate <&> (.eneDate)),
      Se.Set Beam.operationalStartDate (operationalDate <&> (.startDate)),
      Se.Set Beam.operationalDays operationalDays,
      Se.Set Beam.placesId placesId,
      Se.Set Beam.rules (Data.Aeson.toJSON <$> rules),
      Se.Set Beam.service service,
      Se.Set Beam.shortDesc shortDesc,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
