{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketBookingBreakup where

import qualified Domain.Types.FRFSQuoteCategory
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingBreakup
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBookingBreakup as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBookingBreakup.FRFSTicketBookingBreakup -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSTicketBookingBreakup.FRFSTicketBookingBreakup] -> m ())
createMany = traverse_ create

findByTicketBookingIdAndQuoteCategoryId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory -> m (Maybe Domain.Types.FRFSTicketBookingBreakup.FRFSTicketBookingBreakup))
findByTicketBookingIdAndQuoteCategoryId ticketBookingId quoteCategoryId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.ticketBookingId $ Se.Eq (Kernel.Types.Id.getId ticketBookingId),
          Se.Is Beam.quoteCategoryId $ Se.Eq (Kernel.Types.Id.getId quoteCategoryId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingBreakup.FRFSTicketBookingBreakup -> m (Maybe Domain.Types.FRFSTicketBookingBreakup.FRFSTicketBookingBreakup))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBookingBreakup.FRFSTicketBookingBreakup -> m ())
updateByPrimaryKey (Domain.Types.FRFSTicketBookingBreakup.FRFSTicketBookingBreakup {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.quoteCategoryId (Kernel.Types.Id.getId quoteCategoryId),
      Se.Set Beam.tag tag,
      Se.Set Beam.ticketBookingId (Kernel.Types.Id.getId ticketBookingId),
      Se.Set Beam.value value,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSTicketBookingBreakup Domain.Types.FRFSTicketBookingBreakup.FRFSTicketBookingBreakup where
  fromTType' (Beam.FRFSTicketBookingBreakupT {..}) = do
    pure $
      Just
        Domain.Types.FRFSTicketBookingBreakup.FRFSTicketBookingBreakup
          { id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            quoteCategoryId = Kernel.Types.Id.Id quoteCategoryId,
            tag = tag,
            ticketBookingId = Kernel.Types.Id.Id ticketBookingId,
            value = value,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicketBookingBreakup Domain.Types.FRFSTicketBookingBreakup.FRFSTicketBookingBreakup where
  toTType' (Domain.Types.FRFSTicketBookingBreakup.FRFSTicketBookingBreakup {..}) = do
    Beam.FRFSTicketBookingBreakupT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.quoteCategoryId = Kernel.Types.Id.getId quoteCategoryId,
        Beam.tag = tag,
        Beam.ticketBookingId = Kernel.Types.Id.getId ticketBookingId,
        Beam.value = value,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
