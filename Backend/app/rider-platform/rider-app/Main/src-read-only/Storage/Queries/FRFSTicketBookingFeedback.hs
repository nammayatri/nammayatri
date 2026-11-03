{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketBookingFeedback where

import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingFeedback
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBookingFeedback as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBookingFeedback.FRFSTicketBookingFeedback -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSTicketBookingFeedback.FRFSTicketBookingFeedback] -> m ())
createMany = traverse_ create

findByBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m (Maybe Domain.Types.FRFSTicketBookingFeedback.FRFSTicketBookingFeedback))
findByBookingId bookingId = do findOneWithKV [Se.And [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]]

findByMerchantIdAndOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.FRFSTicketBookingFeedback.FRFSTicketBookingFeedback])
findByMerchantIdAndOperatingCityId merchantId merchantOperatingCityId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

updateByBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateByBookingId isFareAccepted feedbackDetails bookingId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.isFareAccepted isFareAccepted,
      Se.Set Beam.feedbackDetails feedbackDetails,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingFeedback.FRFSTicketBookingFeedback -> m (Maybe Domain.Types.FRFSTicketBookingFeedback.FRFSTicketBookingFeedback))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBookingFeedback.FRFSTicketBookingFeedback -> m ())
updateByPrimaryKey (Domain.Types.FRFSTicketBookingFeedback.FRFSTicketBookingFeedback {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bookingId (Kernel.Types.Id.getId bookingId),
      Se.Set Beam.feedbackDetails feedbackDetails,
      Se.Set Beam.isFareAccepted isFareAccepted,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSTicketBookingFeedback Domain.Types.FRFSTicketBookingFeedback.FRFSTicketBookingFeedback where
  fromTType' (Beam.FRFSTicketBookingFeedbackT {..}) = do
    pure $
      Just
        Domain.Types.FRFSTicketBookingFeedback.FRFSTicketBookingFeedback
          { bookingId = Kernel.Types.Id.Id bookingId,
            createdAt = createdAt,
            feedbackDetails = feedbackDetails,
            id = Kernel.Types.Id.Id id,
            isFareAccepted = isFareAccepted,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicketBookingFeedback Domain.Types.FRFSTicketBookingFeedback.FRFSTicketBookingFeedback where
  toTType' (Domain.Types.FRFSTicketBookingFeedback.FRFSTicketBookingFeedback {..}) = do
    Beam.FRFSTicketBookingFeedbackT
      { Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.createdAt = createdAt,
        Beam.feedbackDetails = feedbackDetails,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isFareAccepted = isFareAccepted,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.updatedAt = updatedAt
      }
