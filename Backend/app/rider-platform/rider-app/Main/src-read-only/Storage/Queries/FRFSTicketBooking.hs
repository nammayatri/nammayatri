{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketBooking (module Storage.Queries.FRFSTicketBooking, module ReExport) where

import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus
import qualified Domain.Types.Journey
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.JourneyLeg.Types
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBooking as Beam
import Storage.Queries.FRFSTicketBookingExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSTicketBooking.FRFSTicketBooking] -> m ())
createMany = traverse_ create

findAllByJourneyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Journey.Journey) -> m [Domain.Types.FRFSTicketBooking.FRFSTicketBooking])
findAllByJourneyId journeyId = do findAllWithKV [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId <$> journeyId)]

findAllByJourneyIdCond ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Journey.Journey) -> m [Domain.Types.FRFSTicketBooking.FRFSTicketBooking])
findAllByJourneyIdCond journeyId = do findAllWithKVAndConditionalDB [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId <$> journeyId)] Nothing

findAllByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBookingStatus.FRFSTicketBookingStatus -> m [Domain.Types.FRFSTicketBooking.FRFSTicketBooking])
findAllByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findByBppOrderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findByBppOrderId bppOrderId = do findOneWithKV [Se.Is Beam.bppOrderId $ Se.Eq bppOrderId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m (Maybe Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByQuoteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m (Maybe Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findByQuoteId quoteId = do findOneWithKV [Se.Is Beam.quoteId $ Se.Eq (Kernel.Types.Id.getId quoteId)]

findBySearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m (Maybe Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findBySearchId searchId = do findOneWithKV [Se.Is Beam.searchId $ Se.Eq (Kernel.Types.Id.getId searchId)]

updateBPPOrderIdAndStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.FRFSTicketBookingStatus.FRFSTicketBookingStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateBPPOrderIdAndStatusById bppOrderId status id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.bppOrderId bppOrderId, Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateBPPOrderIdById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateBPPOrderIdById bppOrderId id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.bppOrderId bppOrderId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateBppBankDetailsById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateBppBankDetailsById bppBankAccountNumber bppBankCode id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.bppBankAccountNumber bppBankAccountNumber, Se.Set Beam.bppBankCode bppBankCode, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateCustomerCancelledByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateCustomerCancelledByBookingId customerCancelled id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.customerCancelled customerCancelled, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateFinalPriceById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.Price -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateFinalPriceById finalPrice id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.finalPrice (Kernel.Prelude.fmap (.amount) finalPrice), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateGoogleWalletLinkById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateGoogleWalletLinkById googleWalletJWTUrl id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.googleWalletJWTUrl googleWalletJWTUrl, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateInterestDelayedByQuoteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m ())
updateInterestDelayedByQuoteId bppDelayedInterest quoteId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.bppDelayedInterest bppDelayedInterest, Se.Set Beam.updatedAt _now] [Se.Is Beam.quoteId $ Se.Eq (Kernel.Types.Id.getId quoteId)]

updateIsBookingCancellableByBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateIsBookingCancellableByBookingId isBookingCancellable id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.isBookingCancellable isBookingCancellable, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateJourneyLegStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Lib.JourneyLeg.Types.JourneyLegStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateJourneyLegStatus journeyLegStatus id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.journeyLegStatus journeyLegStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateOnInitDone :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateOnInitDone journeyOnInitDone id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.journeyOnInitDone journeyOnInitDone, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePayoutOrderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updatePayoutOrderId cashbackPayoutOrderId id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.cashbackPayoutOrderId cashbackPayoutOrderId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePayoutStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.FRFSTicketBooking.CashbackStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updatePayoutStatusById cashbackStatus id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.cashbackStatus cashbackStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePriceAndQuantityById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Common.Price -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updatePriceAndQuantityById price quantity childTicketQuantity id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) price),
      Se.Set Beam.price ((.amount) price),
      Se.Set Beam.quantity quantity,
      Se.Set Beam.childTicketQuantity childTicketQuantity,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateQuantity :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateQuantity quantity id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.quantity quantity, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateRefundCancellationChargesAndIsCancellableByBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateRefundCancellationChargesAndIsCancellableByBookingId refundAmount cancellationCharges isBookingCancellable id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.refundAmount refundAmount,
      Se.Set Beam.cancellationCharges cancellationCharges,
      Se.Set Beam.isBookingCancellable isBookingCancellable,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FRFSTicketBookingStatus.FRFSTicketBookingStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusValidTillAndPaymentTxnById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FRFSTicketBookingStatus.FRFSTicketBookingStatus -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateStatusValidTillAndPaymentTxnById status validTill paymentTxnId id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.validTill validTill, Se.Set Beam.paymentTxnId paymentTxnId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateValidTillById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateValidTillById validTill id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.validTill validTill, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m (Maybe Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateByPrimaryKey (Domain.Types.FRFSTicketBooking.FRFSTicketBooking {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam._type _type,
      Se.Set Beam.bookingAuthCode bookingAuthCode,
      Se.Set Beam.bppBankAccountNumber bppBankAccountNumber,
      Se.Set Beam.bppBankCode bppBankCode,
      Se.Set Beam.bppDelayedInterest bppDelayedInterest,
      Se.Set Beam.bppItemId bppItemId,
      Se.Set Beam.bppOrderId bppOrderId,
      Se.Set Beam.bppSubscriberId bppSubscriberId,
      Se.Set Beam.bppSubscriberUrl bppSubscriberUrl,
      Se.Set Beam.cancellationCharges cancellationCharges,
      Se.Set Beam.cashbackPayoutOrderId cashbackPayoutOrderId,
      Se.Set Beam.cashbackStatus cashbackStatus,
      Se.Set Beam.childTicketQuantity childTicketQuantity,
      Se.Set Beam.customerCancelled customerCancelled,
      Se.Set Beam.discountedTickets discountedTickets,
      Se.Set Beam.discountsJson discountsJson,
      Se.Set Beam.estimatedPrice ((.amount) estimatedPrice),
      Se.Set Beam.eventDiscountAmount eventDiscountAmount,
      Se.Set Beam.finalPrice (Kernel.Prelude.fmap (.amount) finalPrice),
      Se.Set Beam.fromStationId fromStationCode,
      Se.Set Beam.googleWalletJWTUrl googleWalletJWTUrl,
      Se.Set Beam.integratedBppConfigId (Kernel.Types.Id.getId integratedBppConfigId),
      Se.Set Beam.isBookingCancellable isBookingCancellable,
      Se.Set Beam.isDeleted isDeleted,
      Se.Set Beam.isFareChanged isFareChanged,
      Se.Set Beam.isSkipped isSkipped,
      Se.Set Beam.journeyId (Kernel.Types.Id.getId <$> journeyId),
      Se.Set Beam.journeyLegId (Kernel.Types.Id.getId <$> journeyLegId),
      Se.Set Beam.journeyLegOrder journeyLegOrder,
      Se.Set Beam.journeyLegStatus journeyLegStatus,
      Se.Set Beam.journeyOnInitDone journeyOnInitDone,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.osBuildVersion osBuildVersion,
      Se.Set Beam.osType osType,
      Se.Set Beam.partnerOrgId (Kernel.Types.Id.getId <$> partnerOrgId),
      Se.Set Beam.partnerOrgTransactionId (Kernel.Types.Id.getId <$> partnerOrgTransactionId),
      Se.Set Beam.payerVpa payerVpa,
      Se.Set Beam.paymentTxnId paymentTxnId,
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) price),
      Se.Set Beam.price ((.amount) price),
      Se.Set Beam.providerDescription providerDescription,
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerName providerName,
      Se.Set Beam.quantity quantity,
      Se.Set Beam.quoteId (Kernel.Types.Id.getId quoteId),
      Se.Set Beam.recentLocationId (Kernel.Types.Id.getId <$> recentLocationId),
      Se.Set Beam.refundAmount refundAmount,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.routeStationsJson routeStationsJson,
      Se.Set Beam.searchId (Kernel.Types.Id.getId searchId),
      Se.Set Beam.startTime startTime,
      Se.Set Beam.stationsJson stationsJson,
      Se.Set Beam.status status,
      Se.Set Beam.toStationId toStationCode,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
