{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CallbackRequest where

import Data.Aeson
import qualified Domain.Types.Merchant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CallbackRequestE e = CallbackRequest
  { id :: Kernel.Types.Id.Id Domain.Types.CallbackRequest.CallbackRequest,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    customerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerPhone :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    customerMobileCountryCode :: Kernel.Prelude.Text,
    status :: Domain.Types.CallbackRequest.CallbackRequestStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type CallbackRequest = CallbackRequestE 'AsEncrypted

type DecryptedCallbackRequest = CallbackRequestE 'AsUnencrypted

instance EncryptedItem CallbackRequest where
  type Unencrypted CallbackRequest = (DecryptedCallbackRequest, HashSalt)
  encryptItem (entity, salt) = do
    customerPhone_ <- encryptItem (customerPhone entity, salt)
    pure
      CallbackRequest
        { id = id entity,
          merchantId = merchantId entity,
          customerName = customerName entity,
          customerPhone = customerPhone_,
          customerMobileCountryCode = customerMobileCountryCode entity,
          status = status entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    customerPhone_ <- fst <$> decryptItem (customerPhone entity)
    pure
      ( CallbackRequest
          { id = id entity,
            merchantId = merchantId entity,
            customerName = customerName entity,
            customerPhone = customerPhone_,
            customerMobileCountryCode = customerMobileCountryCode entity,
            status = status entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' CallbackRequest where
  type UnencryptedItem CallbackRequest = DecryptedCallbackRequest
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data CallbackRequestStatus = PENDING | RESOLVED | CLOSED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CallbackRequestStatus)
