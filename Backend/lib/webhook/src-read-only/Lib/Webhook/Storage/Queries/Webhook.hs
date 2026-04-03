{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Lib.Webhook.Storage.Queries.Webhook (module Lib.Webhook.Storage.Queries.Webhook, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Lib.Webhook.Storage.Queries.WebhookExtra as ReExport
import qualified Lib.Webhook.Types.Webhook
import qualified Lib.Webhook.Storage.Beam.Webhook as Beam
import qualified Kernel.Types.Id
import qualified Lib.Webhook.Storage.Beam.BeamFlow
import qualified Sequelize as Se



create :: (Lib.Webhook.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Webhook.Types.Webhook.Webhook -> m ())
create = createWithKV
createMany :: (Lib.Webhook.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Webhook.Types.Webhook.Webhook] -> m ())
createMany = traverse_ create
findByPrimaryKey :: (Lib.Webhook.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Webhook.Types.Webhook.Webhook -> m (Maybe Lib.Webhook.Types.Webhook.Webhook))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (Lib.Webhook.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Webhook.Types.Webhook.Webhook -> m ())
updateByPrimaryKey (Lib.Webhook.Types.Webhook.Webhook {..}) = do {_now <- getCurrentTime;
                                                                  updateWithKV [Se.Set Beam.batchId batchId,
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
                                                                                Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



