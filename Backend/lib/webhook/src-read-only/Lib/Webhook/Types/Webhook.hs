{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Webhook.Types.Webhook where

import qualified Data.Aeson
import qualified Domain.Types.WebhookExtra
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id

data Webhook = Webhook
  { batchId :: Kernel.Prelude.Text,
    city :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    eventName :: Domain.Types.WebhookExtra.WebhookEvent,
    extMerchantName :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Webhook.Types.Webhook.Webhook,
    lastTriedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Text,
    mode :: Domain.Types.WebhookExtra.WebhookDeliveryType,
    responseCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    responseMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    retryCount :: Kernel.Prelude.Int,
    shortId :: Kernel.Types.Id.ShortId Lib.Webhook.Types.Webhook.Webhook,
    status :: Domain.Types.WebhookExtra.WebhookStatus,
    webhookData :: Data.Aeson.Value,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)
