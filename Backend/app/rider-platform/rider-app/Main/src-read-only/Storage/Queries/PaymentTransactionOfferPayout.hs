{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PaymentTransactionOfferPayout where

import qualified Domain.Types.PaymentTransactionOfferPayout
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PaymentTransactionOfferPayout as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PaymentTransactionOfferPayout.PaymentTransactionOfferPayout -> m ())
create = createWithKV

instance FromTType' Beam.PaymentTransactionOfferPayout Domain.Types.PaymentTransactionOfferPayout.PaymentTransactionOfferPayout where
  fromTType' (Beam.PaymentTransactionOfferPayoutT {..}) = do
    pure $
      Just
        Domain.Types.PaymentTransactionOfferPayout.PaymentTransactionOfferPayout
          { amountPaidByUser = amountPaidByUser,
            bookingCreatedAt = bookingCreatedAt,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            isConsumedByCron = isConsumedByCron,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            networkOrderId = networkOrderId,
            offerCode = offerCode,
            offerId = offerId,
            orderAmount = orderAmount,
            paymentRRN = paymentRRN,
            paymentTxnId = paymentTxnId,
            providerAgency = providerAgency,
            settledAmount = settledAmount,
            settlementReference = settlementReference,
            settlementTs = settlementTs,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PaymentTransactionOfferPayout Domain.Types.PaymentTransactionOfferPayout.PaymentTransactionOfferPayout where
  toTType' (Domain.Types.PaymentTransactionOfferPayout.PaymentTransactionOfferPayout {..}) = do
    Beam.PaymentTransactionOfferPayoutT
      { Beam.amountPaidByUser = amountPaidByUser,
        Beam.bookingCreatedAt = bookingCreatedAt,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isConsumedByCron = isConsumedByCron,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.networkOrderId = networkOrderId,
        Beam.offerCode = offerCode,
        Beam.offerId = offerId,
        Beam.orderAmount = orderAmount,
        Beam.paymentRRN = paymentRRN,
        Beam.paymentTxnId = paymentTxnId,
        Beam.providerAgency = providerAgency,
        Beam.settledAmount = settledAmount,
        Beam.settlementReference = settlementReference,
        Beam.settlementTs = settlementTs,
        Beam.updatedAt = updatedAt
      }
