{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSBookingFareBreakUp where

import qualified Domain.Types.FRFSBookingFareBreakUp
import qualified Domain.Types.FRFSTicketBooking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSBookingFareBreakUp as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSBookingFareBreakUp.FRFSBookingFareBreakUp -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSBookingFareBreakUp.FRFSBookingFareBreakUp] -> m ())
createMany = traverse_ create

deleteByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
deleteByBookingId bookingId = do deleteWithKV [Se.And [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]]

findAllByBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ([Domain.Types.FRFSBookingFareBreakUp.FRFSBookingFareBreakUp]))
findAllByBookingId bookingId = do findAllWithKVAndConditionalDB [Se.And [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]] Nothing

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSBookingFareBreakUp.FRFSBookingFareBreakUp -> m (Maybe Domain.Types.FRFSBookingFareBreakUp.FRFSBookingFareBreakUp))
findById id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSBookingFareBreakUp.FRFSBookingFareBreakUp -> m (Maybe Domain.Types.FRFSBookingFareBreakUp.FRFSBookingFareBreakUp))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSBookingFareBreakUp.FRFSBookingFareBreakUp -> m ())
updateByPrimaryKey (Domain.Types.FRFSBookingFareBreakUp.FRFSBookingFareBreakUp {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount (((.amount) amount)),
      Se.Set Beam.currency ((Just $ (.currency) amount)),
      Se.Set Beam.bookingId (Kernel.Types.Id.getId bookingId),
      Se.Set Beam.description description,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSBookingFareBreakUp Domain.Types.FRFSBookingFareBreakUp.FRFSBookingFareBreakUp where
  fromTType' (Beam.FRFSBookingFareBreakUpT {..}) = do
    pure $
      Just
        Domain.Types.FRFSBookingFareBreakUp.FRFSBookingFareBreakUp
          { amount = Kernel.Types.Common.mkPrice currency amount,
            bookingId = Kernel.Types.Id.Id bookingId,
            description = description,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSBookingFareBreakUp Domain.Types.FRFSBookingFareBreakUp.FRFSBookingFareBreakUp where
  toTType' (Domain.Types.FRFSBookingFareBreakUp.FRFSBookingFareBreakUp {..}) = do
    Beam.FRFSBookingFareBreakUpT
      { Beam.amount = ((.amount) amount),
        Beam.currency = (Just $ (.currency) amount),
        Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
