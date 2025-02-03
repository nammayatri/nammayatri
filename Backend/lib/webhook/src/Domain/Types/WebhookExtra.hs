{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.WebhookExtra where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.External.Encryption
import Kernel.Prelude

data ExternalWebhookConfigs = ExternalWebhookConfigs
  { baseUrl :: BaseUrl,
    apiKey :: Maybe (EncryptedField 'AsEncrypted Text),
    merchantId :: Maybe Text,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data WebhookEvent
  = MANDATE
  | SERVICE_STARTED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data WebhookStatus = DELIVERED | FAILED | PENDING | RETRIES_ENDED | NO_CONFIG deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data WebhookDeliveryType = BATCHING | REAL_TIME deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data ExternalWebhookData = ExternalWebhookData
  { id :: Text,
    shortId :: Text,
    eventName :: WebhookEvent,
    webhookData :: Value
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''WebhookEvent)
$(mkBeamInstancesForEnumAndList ''WebhookDeliveryType)
$(mkBeamInstancesForEnumAndList ''WebhookStatus)
