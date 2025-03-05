{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketBookingPeopleCategory (module Storage.Queries.TicketBookingPeopleCategory, module ReExport) where

import qualified Data.Aeson
import qualified Domain.Types.TicketBookingPeopleCategory
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
import qualified Storage.Beam.TicketBookingPeopleCategory as Beam
import Storage.Queries.TicketBookingPeopleCategoryExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory] -> m ())
createMany = traverse_ create

findAllByServiceCategoryId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory -> m [Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory])
findAllByServiceCategoryId ticketBookingServiceCategoryId = do findAllWithKV [Se.Is Beam.ticketBookingServiceCategoryId $ Se.Eq (Kernel.Types.Id.getId ticketBookingServiceCategoryId)]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory -> m (Maybe Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateTicketPeopleUnitsAndAmountById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory -> m ())
updateTicketPeopleUnitsAndAmountById numberOfUnitsCancelled amountToRefund id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.numberOfUnitsCancelled numberOfUnitsCancelled, Se.Set Beam.amountToRefund amountToRefund, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory -> m (Maybe Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory -> m ())
updateByPrimaryKey (Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amountToRefund amountToRefund,
      Se.Set Beam.name name,
      Se.Set Beam.numberOfUnits numberOfUnits,
      Se.Set Beam.numberOfUnitsCancelled numberOfUnitsCancelled,
      Se.Set Beam.peopleCategoryId (Kernel.Types.Id.getId <$> peopleCategoryId),
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) pricePerUnit),
      Se.Set Beam.pricePerUnit ((.amount) pricePerUnit),
      Se.Set Beam.ticketBookingServiceCategoryId (Kernel.Types.Id.getId ticketBookingServiceCategoryId),
      Se.Set Beam.vendorSplitDetails (Data.Aeson.toJSON <$> vendorSplitDetails),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
