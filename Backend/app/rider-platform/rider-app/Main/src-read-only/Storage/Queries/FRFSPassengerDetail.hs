{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSPassengerDetail where

import qualified Domain.Types.FRFSPassengerDetail
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSTicketBooking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSPassengerDetail as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail] -> m ())
createMany = traverse_ create

deleteAllByQuoteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m ())
deleteAllByQuoteId quoteId = do deleteWithKV [Se.Is Beam.quoteId $ Se.Eq (Kernel.Types.Id.getId quoteId)]

findAllByBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking) -> m [Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail])
findAllByBookingId bookingId = do findAllWithKV [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId <$> bookingId)]

findAllByQuoteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m [Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail])
findAllByQuoteId quoteId = do findAllWithKV [Se.Is Beam.quoteId $ Se.Eq (Kernel.Types.Id.getId quoteId)]

updateBookingIdByQuoteId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking) -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m ())
updateBookingIdByQuoteId bookingId quoteId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.bookingId (Kernel.Types.Id.getId <$> bookingId), Se.Set Beam.updatedAt _now] [Se.Is Beam.quoteId $ Se.Eq (Kernel.Types.Id.getId quoteId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail -> m (Maybe Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail -> m ())
updateByPrimaryKey (Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.age age,
      Se.Set Beam.bookingId (Kernel.Types.Id.getId <$> bookingId),
      Se.Set Beam.dropOffPointPlaceId dropOffPointPlaceId,
      Se.Set Beam.gender gender,
      Se.Set Beam.idProofLookupId idProofLookupId,
      Se.Set Beam.idProofNumberEncrypted (idProofNumber <&> unEncrypted . encrypted),
      Se.Set Beam.idProofNumberHash (idProofNumber <&> hash),
      Se.Set Beam.isChild isChild,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.pickupPointPlaceId pickupPointPlaceId,
      Se.Set Beam.quoteId (Kernel.Types.Id.getId quoteId),
      Se.Set Beam.seatId (Kernel.Types.Id.getId seatId),
      Se.Set Beam.seatLabel seatLabel,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSPassengerDetail Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail where
  fromTType' (Beam.FRFSPassengerDetailT {..}) = do
    pure $
      Just
        Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail
          { age = age,
            bookingId = Kernel.Types.Id.Id <$> bookingId,
            dropOffPointPlaceId = dropOffPointPlaceId,
            gender = gender,
            id = Kernel.Types.Id.Id id,
            idProofLookupId = idProofLookupId,
            idProofNumber = EncryptedHashed <$> (Encrypted <$> idProofNumberEncrypted) <*> idProofNumberHash,
            isChild = isChild,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            pickupPointPlaceId = pickupPointPlaceId,
            quoteId = Kernel.Types.Id.Id quoteId,
            seatId = Kernel.Types.Id.Id seatId,
            seatLabel = seatLabel,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSPassengerDetail Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail where
  toTType' (Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail {..}) = do
    Beam.FRFSPassengerDetailT
      { Beam.age = age,
        Beam.bookingId = Kernel.Types.Id.getId <$> bookingId,
        Beam.dropOffPointPlaceId = dropOffPointPlaceId,
        Beam.gender = gender,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.idProofLookupId = idProofLookupId,
        Beam.idProofNumberEncrypted = idProofNumber <&> unEncrypted . encrypted,
        Beam.idProofNumberHash = idProofNumber <&> hash,
        Beam.isChild = isChild,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.pickupPointPlaceId = pickupPointPlaceId,
        Beam.quoteId = Kernel.Types.Id.getId quoteId,
        Beam.seatId = Kernel.Types.Id.getId seatId,
        Beam.seatLabel = seatLabel,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
