{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.RsfOrderStateExtra
  ( findAllByMerchantAndOrderIds,
    createMissing,
    findPageByMerchantAndLastClaimedAtRange,
    findAllByMerchantAndLastClaimedAtRange,
    upsertReported,
  )
where

import qualified Data.HashSet as HS
import Data.List (nub, partition)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import qualified Lib.Finance.Domain.Types.RsfOrderState as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.RsfOrderState as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.RsfOrderState ()
import qualified Sequelize as Se

findAllByMerchantAndOrderIds ::
  (BeamFlow m r) =>
  Text ->
  [Text] ->
  m [Domain.RsfOrderState]
findAllByMerchantAndOrderIds _ [] = pure []
findAllByMerchantAndOrderIds merchantId orderIds =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.orderId $ Se.In orderIds
        ]
    ]

-- Phase 2: one row per order the first time a receiver_recon names it (reportedStatus NULL), and
-- lastClaimedAt bumped on every later receiver_recon that names it again, so the dashboard lists the
-- order under the day of its latest claim. One batched read, inserts for the missing ids, one update
-- for the rest.
createMissing :: (BeamFlow m r) => Text -> Maybe Text -> UTCTime -> [Text] -> m ()
createMissing merchantId bapId now ids = do
  existing <- findAllByMerchantAndOrderIds merchantId ids
  let seen = HS.fromList (map (.orderId) existing)
      (known, fresh) = partition (`HS.member` seen) (nub ids)
  unless (null known) $
    updateWithKV
      [Se.Set Beam.lastClaimedAt (Just now), Se.Set Beam.updatedAt now]
      [Se.And [Se.Is Beam.merchantId $ Se.Eq merchantId, Se.Is Beam.orderId $ Se.In known]]
  traverse_
    createWithKV
    [ Domain.RsfOrderState
        { merchantId = merchantId,
          orderId = orderId,
          bapId = bapId,
          reportedStatus = Nothing,
          reportedDiff = Nothing,
          reportedCode = Nothing,
          lastReportedMessageId = Nothing,
          lastReportedAt = Nothing,
          lastClaimedAt = Just now,
          createdAt = now,
          updatedAt = now
        }
      | orderId <- fresh
    ]

-- Dashboard paging: orders whose latest claim falls in the range, oldest first, one DB page.
findPageByMerchantAndLastClaimedAtRange :: (BeamFlow m r) => Text -> UTCTime -> UTCTime -> Maybe Text -> Int -> Int -> m [Domain.RsfOrderState]
findPageByMerchantAndLastClaimedAtRange merchantId from to mbBapId limit offset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.lastClaimedAt $ Se.GreaterThanOrEq (Just from),
          Se.Is Beam.lastClaimedAt $ Se.LessThan (Just to)
        ]
          <> [Se.Is Beam.bapId $ Se.Eq (Just b) | Just b <- [mbBapId]]
    ]
    (Se.Asc Beam.lastClaimedAt)
    (Just limit)
    (Just offset)

-- Every order whose latest claim falls in the range (the day summary folds the whole day).
findAllByMerchantAndLastClaimedAtRange :: (BeamFlow m r) => Text -> UTCTime -> UTCTime -> m [Domain.RsfOrderState]
findAllByMerchantAndLastClaimedAtRange merchantId from to =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.lastClaimedAt $ Se.GreaterThanOrEq (Just from),
          Se.Is Beam.lastClaimedAt $ Se.LessThan (Just to)
        ]
    ]

-- Phase 4 (Mapping C): stamped once per send, only after the collector ACKs.
upsertReported ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  Text ->
  HighPrecMoney ->
  Maybe Text ->
  Text ->
  UTCTime ->
  m ()
upsertReported merchantId orderId reportedStatus reportedDiff reportedCode messageId reportedAt = do
  let key =
        [ Se.And
            [ Se.Is Beam.merchantId $ Se.Eq merchantId,
              Se.Is Beam.orderId $ Se.Eq orderId
            ]
        ]
  existing <- findOneWithKV key
  case (existing :: Maybe Domain.RsfOrderState) of
    Just _ ->
      updateWithKV
        [ Se.Set Beam.reportedStatus (Just reportedStatus),
          Se.Set Beam.reportedDiff (Just reportedDiff),
          Se.Set Beam.reportedCode reportedCode,
          Se.Set Beam.lastReportedMessageId (Just messageId),
          Se.Set Beam.lastReportedAt (Just reportedAt),
          Se.Set Beam.updatedAt reportedAt
        ]
        key
    Nothing ->
      createWithKV
        Domain.RsfOrderState
          { merchantId = merchantId,
            orderId = orderId,
            bapId = Nothing,
            reportedStatus = Just reportedStatus,
            reportedDiff = Just reportedDiff,
            reportedCode = reportedCode,
            lastReportedMessageId = Just messageId,
            lastReportedAt = Just reportedAt,
            lastClaimedAt = Just reportedAt,
            createdAt = reportedAt,
            updatedAt = reportedAt
          }
