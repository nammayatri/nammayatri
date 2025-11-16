{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Webhook.Storage.Queries.Webhook (module Lib.Webhook.Storage.Queries.Webhook, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Webhook.Storage.Beam.BeamFlow
import qualified Lib.Webhook.Storage.Beam.Webhook as Beam
import Lib.Webhook.Storage.Queries.WebhookExtra as ReExport
import qualified Lib.Webhook.Types.Webhook
import qualified Sequelize as Se

create :: (Lib.Webhook.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Webhook.Types.Webhook.Webhook -> m ())
create = createWithKV

createMany :: (Lib.Webhook.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Webhook.Types.Webhook.Webhook] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (Lib.Webhook.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Webhook.Types.Webhook.Webhook -> m (Maybe Lib.Webhook.Types.Webhook.Webhook))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Webhook.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Webhook.Types.Webhook.Webhook -> m ())
updateByPrimaryKey (Lib.Webhook.Types.Webhook.Webhook {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.batchId batchId,
      Se.Set Beam.city city,
      Se.Set Beam.eventName eventName,
      Se.Set Beam.extMerchantName extMerchantName,
      Se.Set Beam.lastTriedAt lastTriedAt,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.mode mode,
      Se.Set Beam.responseCode responseCode,
      Se.Set Beam.responseMessage responseMessage,
      Se.Set Beam.retryCount retryCount,
      Se.Set Beam.shortId (Kernel.Types.Id.getShortId shortId),
      Se.Set Beam.status status,
      Se.Set Beam.webhookData webhookData,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
