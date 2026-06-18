module Lib.Finance.FinanceEvents.Publisher
  ( FinanceEventsPublisherCfg (..),
    publishLedgerAccountUpdate,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable (hash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Storage.Hedis.Config (HedisFlow)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (logError, logInfo)
import Kernel.Utils.Dhall (FromDhall)
import Lib.Finance.Domain.Types.LedgerEntry (LedgerEntry)

data FinanceEventsPublisherCfg = FinanceEventsPublisherCfg
  { streamPrefix :: Text,
    shardCount :: Int
  }
  deriving (Generic, FromDhall, Show)

publishLedgerAccountUpdate ::
  ( HedisFlow m r,
    HasField "financeEventsPublisherCfg" r (Maybe FinanceEventsPublisherCfg),
    MonadIO m
  ) =>
  Id LedgerEntry ->
  m ()
publishLedgerAccountUpdate entryId = do
  mbCfg <- asks (.financeEventsPublisherCfg)
  case mbCfg of
    Nothing -> pure ()
    Just cfg -> do
      result <- try @_ @SomeException $ do
        let entryIdT = entryId.getId
            shardId = hash entryIdT `mod` cfg.shardCount
            streamName = cfg.streamPrefix <> show shardId
            payload = BSL.toStrict (Aeson.encode entryIdT)
        void $ Hedis.xAdd streamName "*" [("payload", payload)]
        logInfo $ "finance-events.published entryId=" <> entryIdT <> " stream=" <> streamName
      case result of
        Right () -> pure ()
        Left e -> logError $ "finance-events.publish-failed entryId=" <> entryId.getId <> " err=" <> show e
