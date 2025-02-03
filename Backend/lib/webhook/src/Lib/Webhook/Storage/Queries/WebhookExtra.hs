{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Webhook.Storage.Queries.WebhookExtra where

import qualified Data.Aeson as A
import qualified Domain.Types.WebhookExtra as WT
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Webhook.Storage.Beam.BeamFlow as BF
import qualified Lib.Webhook.Storage.Beam.Webhook as BeamWeb
import Lib.Webhook.Storage.Queries.OrphanInstances.Webhook
import Lib.Webhook.Types.Webhook
import qualified Sequelize as Se

findAllWithStatusModeWithinRetryThreshold ::
  (BF.BeamFlow m r) =>
  [WT.WebhookStatus] ->
  WT.WebhookDeliveryType ->
  Int ->
  Maybe WT.WebhookEvent ->
  Maybe (Id Webhook) ->
  Maybe Text ->
  Int ->
  m [Webhook]
findAllWithStatusModeWithinRetryThreshold status deliveryMode retryCount event mbWebhookId batchId limit = do
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamWeb.status $ Se.In status,
          Se.Is BeamWeb.mode $ Se.Eq deliveryMode,
          Se.Is BeamWeb.retryCount $ Se.LessThanOrEq retryCount
        ]
          <> [Se.Is BeamWeb.eventName $ Se.Eq $ fromMaybe WT.MANDATE event | isJust event]
          <> [Se.Is BeamWeb.id $ Se.Eq $ maybe "" getId mbWebhookId | isJust mbWebhookId]
          <> [Se.Is BeamWeb.batchId $ Se.Eq $ fromMaybe "" batchId | isJust batchId]
    ]
    (Se.Asc BeamWeb.createdAt)
    (Just limit)
    Nothing

updateRetryCount ::
  (BF.BeamFlow m r) =>
  Int ->
  Id Webhook ->
  m ()
updateRetryCount retryCount webhookId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamWeb.retryCount retryCount,
      Se.Set BeamWeb.updatedAt now
    ]
    [Se.Is BeamWeb.id $ Se.Eq webhookId.getId]

updateWebhookStatus ::
  (BF.BeamFlow m r) =>
  WT.WebhookStatus ->
  Id Webhook ->
  m ()
updateWebhookStatus status webhookId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamWeb.status status,
      Se.Set BeamWeb.updatedAt now
    ]
    [Se.Is BeamWeb.id $ Se.Eq webhookId.getId]

updateWebhookLastTriedAt ::
  (BF.BeamFlow m r) =>
  UTCTime ->
  Id Webhook ->
  m ()
updateWebhookLastTriedAt time webhookId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamWeb.lastTriedAt time,
      Se.Set BeamWeb.updatedAt now
    ]
    [Se.Is BeamWeb.id $ Se.Eq webhookId.getId]
