{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.Payout.History
  ( PayoutHistoryEvent (..),
    PayoutHistoryRecord (..),
    recordPayoutHistory,
    getPayoutHistory,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKeyMap
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (generateGUID, getCurrentTime)
import qualified Lib.Finance.Domain.Types.StateTransition as ST
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.StateTransition as QTransition

data PayoutHistoryRecord = PayoutHistoryRecord
  { entityType :: Text,
    entityId :: Text,
    fromState :: Maybe ST.PaymentState,
    toState :: ST.PaymentState,
    message :: Maybe Text,
    metadata :: Maybe A.Value,
    actorType :: Text,
    actorId :: Maybe Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text
  }
  deriving (Generic, Show)

data PayoutHistoryEvent = PayoutHistoryEvent
  { status :: Text,
    message :: Maybe Text,
    timestamp :: UTCTime
  }
  deriving (Generic, Show)

recordPayoutHistory ::
  (BeamFlow.BeamFlow m r) =>
  PayoutHistoryRecord ->
  m ST.StateTransition
recordPayoutHistory PayoutHistoryRecord {..} =
  recordTransition
    entityType
    entityId
    fromState
    toState
    message
    metadata
    actorType
    actorId
    merchantId
    merchantOperatingCityId

getPayoutHistory ::
  (BeamFlow.BeamFlow m r) =>
  Text ->
  Text ->
  m [PayoutHistoryEvent]
getPayoutHistory entityType entityId = do
  history <- QTransition.findByEntity entityType entityId
  let sortedHistory = List.sortOn (Ord.Down . (.createdAt)) history
  pure $ map toEvent sortedHistory
  where
    toEvent ST.StateTransition {toState, eventData, createdAt} =
      PayoutHistoryEvent
        { status = T.pack (show toState),
          message = extractMessage eventData,
          timestamp = createdAt
        }

recordTransition ::
  (BeamFlow.BeamFlow m r) =>
  Text ->
  Text ->
  Maybe ST.PaymentState ->
  ST.PaymentState ->
  Maybe Text ->
  Maybe A.Value ->
  Text ->
  Maybe Text ->
  Text ->
  Text ->
  m ST.StateTransition
recordTransition entityType entityId fromState toState message metadata actorType actorId merchantId merchantOperatingCityId = do
  now <- getCurrentTime
  transitionId <- Id <$> generateGUID
  let fromState' = fromMaybe toState fromState
      eventData' = mkEventData message metadata
      transition =
        ST.StateTransition
          { id = transitionId,
            entityType = entityType,
            entityId = entityId,
            fromState = fromState',
            toState = toState,
            event = ST.PAYOUT_STATUS_CHANGED,
            eventData = eventData',
            actorType = actorType,
            actorId = actorId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
  QTransition.create transition
  pure transition

mkEventData :: Maybe Text -> Maybe A.Value -> Maybe A.Value
mkEventData mbMessage mbMetadata =
  case (mbMessage, mbMetadata) of
    (Nothing, Nothing) -> Nothing
    _ ->
      Just $
        A.Object $
          AKeyMap.fromList $
            catMaybes
              [ (AKey.fromText "message",) . A.String <$> mbMessage,
                (AKey.fromText "metadata",) <$> mbMetadata
              ]

extractMessage :: Maybe A.Value -> Maybe Text
extractMessage = \case
  Just (A.Object obj) ->
    case AKeyMap.lookup (AKey.fromText "message") obj of
      Just (A.String t) -> Just t
      _ -> Nothing
  Just (A.String t) -> Just t
  _ -> Nothing
