{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketBooking where

import qualified Domain.Types.FRFSTicketBooking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBooking as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSTicketBooking.FRFSTicketBooking] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m (Maybe Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateByPrimaryKey (Domain.Types.FRFSTicketBooking.FRFSTicketBooking {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bapId bapId,
      Se.Set Beam.bookingType bookingType,
      Se.Set Beam.bppId bppId,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.quantity quantity,
      Se.Set Beam.selectedFareId (Kernel.Types.Id.getId <$> selectedFareId),
      Se.Set Beam.status status,
      Se.Set Beam.transactionId (Kernel.Types.Id.getId transactionId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSTicketBooking Domain.Types.FRFSTicketBooking.FRFSTicketBooking where
  fromTType' (Beam.FRFSTicketBookingT {..}) = do
    pure $
      Just
        Domain.Types.FRFSTicketBooking.FRFSTicketBooking
          { bapId = bapId,
            bookingType = bookingType,
            bppId = bppId,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            quantity = quantity,
            selectedFareId = Kernel.Types.Id.Id <$> selectedFareId,
            status = status,
            transactionId = Kernel.Types.Id.Id transactionId,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.FRFSTicketBooking Domain.Types.FRFSTicketBooking.FRFSTicketBooking where
  toTType' (Domain.Types.FRFSTicketBooking.FRFSTicketBooking {..}) = do
    Beam.FRFSTicketBookingT
      { Beam.bapId = bapId,
        Beam.bookingType = bookingType,
        Beam.bppId = bppId,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.quantity = quantity,
        Beam.selectedFareId = Kernel.Types.Id.getId <$> selectedFareId,
        Beam.status = status,
        Beam.transactionId = Kernel.Types.Id.getId transactionId,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
