{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Webhook.Storage.Beam.Webhook where
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Beam.Lib.UtilsTH
import qualified Kernel.Prelude
import qualified Domain.Types.WebhookExtra
import qualified Data.Aeson
import qualified Database.Beam as B



data WebhookT f
    = WebhookT {batchId :: (B.C f Kernel.Prelude.Text),
                city :: (B.C f Kernel.Prelude.Text),
                createdAt :: (B.C f Kernel.Prelude.UTCTime),
                eventName :: (B.C f Domain.Types.WebhookExtra.WebhookEvent),
                extMerchantName :: (B.C f Kernel.Prelude.Text),
                id :: (B.C f Kernel.Prelude.Text),
                lastTriedAt :: (B.C f Kernel.Prelude.UTCTime),
                merchantId :: (B.C f Kernel.Prelude.Text),
                mode :: (B.C f Domain.Types.WebhookExtra.WebhookDeliveryType),
                responseCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                responseMessage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                retryCount :: (B.C f Kernel.Prelude.Int),
                shortId :: (B.C f Kernel.Prelude.Text),
                status :: (B.C f Domain.Types.WebhookExtra.WebhookStatus),
                webhookData :: (B.C f Data.Aeson.Value),
                updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table WebhookT
    where data PrimaryKey WebhookT f = WebhookId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = WebhookId . id
type Webhook = WebhookT Identity

$(enableKVPG (''WebhookT) [('id)] [])

$(mkTableInstancesGenericSchema (''WebhookT) "webhook")

