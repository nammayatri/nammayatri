{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSRecon where

import qualified Domain.Types.FRFSRecon
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketStatus
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSRecon as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSRecon.FRFSRecon -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSRecon.FRFSRecon] -> m ())
createMany = traverse_ create

updateStatusByTicketBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.FRFSTicketStatus.FRFSTicketStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateStatusByTicketBookingId ticketStatus frfsTicketBookingId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.ticketStatus ticketStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.frfsTicketBookingId $ Se.Eq (Kernel.Types.Id.getId frfsTicketBookingId)]

updateTOrderValueAndSettlementAmountById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Common.Price -> Kernel.Types.Common.Price -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateTOrderValueAndSettlementAmountById settlementAmount totalOrderValue frfsTicketBookingId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.settlementAmount ((.amount) settlementAmount),
      Se.Set Beam.totalOrderValue ((.amount) totalOrderValue),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.frfsTicketBookingId $ Se.Eq (Kernel.Types.Id.getId frfsTicketBookingId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSRecon.FRFSRecon -> m (Maybe Domain.Types.FRFSRecon.FRFSRecon))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSRecon.FRFSRecon -> m ())
updateByPrimaryKey (Domain.Types.FRFSRecon.FRFSRecon {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.beneficiaryBankAccount beneficiaryBankAccount,
      Se.Set Beam.beneficiaryIFSC beneficiaryIFSC,
      Se.Set Beam.buyerFinderFee ((.amount) buyerFinderFee),
      Se.Set Beam.collectorIFSC collectorIFSC,
      Se.Set Beam.collectorSubscriberId collectorSubscriberId,
      Se.Set Beam.date date,
      Se.Set Beam.destinationStationCode destinationStationCode,
      Se.Set Beam.differenceAmount (Kernel.Prelude.fmap (.amount) differenceAmount),
      Se.Set Beam.entityType entityType,
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) fare),
      Se.Set Beam.fare ((.amount) fare),
      Se.Set Beam.frfsTicketBookingId (Kernel.Types.Id.getId frfsTicketBookingId),
      Se.Set Beam.message message,
      Se.Set Beam.mobileNumber mobileNumber,
      Se.Set Beam.networkOrderId networkOrderId,
      Se.Set Beam.paymentGateway paymentGateway,
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerName providerName,
      Se.Set Beam.receiverSubscriberId receiverSubscriberId,
      Se.Set Beam.reconStatus reconStatus,
      Se.Set Beam.settlementAmount ((.amount) settlementAmount),
      Se.Set Beam.settlementDate settlementDate,
      Se.Set Beam.settlementReferenceNumber settlementReferenceNumber,
      Se.Set Beam.sourceStationCode sourceStationCode,
      Se.Set Beam.ticketNumber ticketNumber,
      Se.Set Beam.ticketQty ticketQty,
      Se.Set Beam.ticketStatus ticketStatus,
      Se.Set Beam.time time,
      Se.Set Beam.totalOrderValue ((.amount) totalOrderValue),
      Se.Set Beam.transactionRefNumber transactionRefNumber,
      Se.Set Beam.transactionUUID transactionUUID,
      Se.Set Beam.txnId txnId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSRecon Domain.Types.FRFSRecon.FRFSRecon where
  fromTType' (Beam.FRFSReconT {..}) = do
    pure $
      Just
        Domain.Types.FRFSRecon.FRFSRecon
          { beneficiaryBankAccount = beneficiaryBankAccount,
            beneficiaryIFSC = beneficiaryIFSC,
            buyerFinderFee = Kernel.Types.Common.mkPrice currency buyerFinderFee,
            collectorIFSC = collectorIFSC,
            collectorSubscriberId = collectorSubscriberId,
            date = date,
            destinationStationCode = destinationStationCode,
            differenceAmount = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) differenceAmount,
            entityType = entityType,
            fare = Kernel.Types.Common.mkPrice currency fare,
            frfsTicketBookingId = Kernel.Types.Id.Id frfsTicketBookingId,
            id = Kernel.Types.Id.Id id,
            message = message,
            mobileNumber = mobileNumber,
            networkOrderId = networkOrderId,
            paymentGateway = paymentGateway,
            providerId = providerId,
            providerName = providerName,
            receiverSubscriberId = receiverSubscriberId,
            reconStatus = reconStatus,
            settlementAmount = Kernel.Types.Common.mkPrice currency settlementAmount,
            settlementDate = settlementDate,
            settlementReferenceNumber = settlementReferenceNumber,
            sourceStationCode = sourceStationCode,
            ticketNumber = ticketNumber,
            ticketQty = ticketQty,
            ticketStatus = ticketStatus,
            time = time,
            totalOrderValue = Kernel.Types.Common.mkPrice currency totalOrderValue,
            transactionRefNumber = transactionRefNumber,
            transactionUUID = transactionUUID,
            txnId = txnId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSRecon Domain.Types.FRFSRecon.FRFSRecon where
  toTType' (Domain.Types.FRFSRecon.FRFSRecon {..}) = do
    Beam.FRFSReconT
      { Beam.beneficiaryBankAccount = beneficiaryBankAccount,
        Beam.beneficiaryIFSC = beneficiaryIFSC,
        Beam.buyerFinderFee = (.amount) buyerFinderFee,
        Beam.collectorIFSC = collectorIFSC,
        Beam.collectorSubscriberId = collectorSubscriberId,
        Beam.date = date,
        Beam.destinationStationCode = destinationStationCode,
        Beam.differenceAmount = Kernel.Prelude.fmap (.amount) differenceAmount,
        Beam.entityType = entityType,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) fare,
        Beam.fare = (.amount) fare,
        Beam.frfsTicketBookingId = Kernel.Types.Id.getId frfsTicketBookingId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.message = message,
        Beam.mobileNumber = mobileNumber,
        Beam.networkOrderId = networkOrderId,
        Beam.paymentGateway = paymentGateway,
        Beam.providerId = providerId,
        Beam.providerName = providerName,
        Beam.receiverSubscriberId = receiverSubscriberId,
        Beam.reconStatus = reconStatus,
        Beam.settlementAmount = (.amount) settlementAmount,
        Beam.settlementDate = settlementDate,
        Beam.settlementReferenceNumber = settlementReferenceNumber,
        Beam.sourceStationCode = sourceStationCode,
        Beam.ticketNumber = ticketNumber,
        Beam.ticketQty = ticketQty,
        Beam.ticketStatus = ticketStatus,
        Beam.time = time,
        Beam.totalOrderValue = (.amount) totalOrderValue,
        Beam.transactionRefNumber = transactionRefNumber,
        Beam.transactionUUID = transactionUUID,
        Beam.txnId = txnId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
