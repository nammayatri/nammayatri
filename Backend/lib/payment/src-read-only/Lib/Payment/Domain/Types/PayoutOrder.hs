{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PayoutOrder where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Payout.Juspay.Types.Payout
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.Common

data PayoutOrderE e = PayoutOrder
  { accountDetailsType :: Kernel.Prelude.Maybe Kernel.External.Payout.Juspay.Types.Payout.AccountDetailsType,
    amount :: Kernel.Types.Common.Price,
    city :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    customerEmail :: (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    customerId :: Kernel.Prelude.Text,
    entityIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    entityName :: Kernel.Prelude.Maybe Lib.Payment.Domain.Types.Common.EntityName,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder,
    lastStatusCheckedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNo :: (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    orderId :: Kernel.Prelude.Text,
    responseCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    responseMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    retriedOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    shortId :: Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder),
    status :: Kernel.External.Payout.Juspay.Types.Payout.PayoutOrderStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    vpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic)

type PayoutOrder = PayoutOrderE ('AsEncrypted)

type DecryptedPayoutOrder = PayoutOrderE ('AsUnencrypted)

instance EncryptedItem PayoutOrder where
  type Unencrypted PayoutOrder = (DecryptedPayoutOrder, HashSalt)
  encryptItem (entity, salt) = do
    customerEmail_ <- encryptItem (customerEmail entity, salt)
    mobileNo_ <- encryptItem (mobileNo entity, salt)
    pure
      PayoutOrder
        { accountDetailsType = accountDetailsType entity,
          amount = amount entity,
          city = city entity,
          createdAt = createdAt entity,
          customerEmail = customerEmail_,
          customerId = customerId entity,
          entityIds = entityIds entity,
          entityName = entityName entity,
          id = id entity,
          lastStatusCheckedAt = lastStatusCheckedAt entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          mobileNo = mobileNo_,
          orderId = orderId entity,
          responseCode = responseCode entity,
          responseMessage = responseMessage entity,
          retriedOrderId = retriedOrderId entity,
          shortId = shortId entity,
          status = status entity,
          updatedAt = updatedAt entity,
          vpa = vpa entity
        }
  decryptItem entity = do
    customerEmail_ <- fst <$> decryptItem (customerEmail entity)
    mobileNo_ <- fst <$> decryptItem (mobileNo entity)
    pure
      ( PayoutOrder
          { accountDetailsType = accountDetailsType entity,
            amount = amount entity,
            city = city entity,
            createdAt = createdAt entity,
            customerEmail = customerEmail_,
            customerId = customerId entity,
            entityIds = entityIds entity,
            entityName = entityName entity,
            id = id entity,
            lastStatusCheckedAt = lastStatusCheckedAt entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            mobileNo = mobileNo_,
            orderId = orderId entity,
            responseCode = responseCode entity,
            responseMessage = responseMessage entity,
            retriedOrderId = retriedOrderId entity,
            shortId = shortId entity,
            status = status entity,
            updatedAt = updatedAt entity,
            vpa = vpa entity
          },
        ""
      )

instance EncryptedItem' PayoutOrder where
  type UnencryptedItem PayoutOrder = DecryptedPayoutOrder
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
