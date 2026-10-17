{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.RsfOrderStateExtra
  ( findAllByMerchantAndOrderIds,
    createMissing,
    findPageByMerchantAndCreatedAtRange,
    findAllByMerchantAndCreatedAtRange,
    upsertReported,
  )
where

import qualified Data.HashSet as HS
import Data.List (nub)
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

-- Phase 2: one row per order the first time a receiver_recon claims it, reportedStatus NULL.
-- One batched read for the ids already present, then inserts only for the missing ones.
createMissing :: (BeamFlow m r) => Text -> Maybe Text -> UTCTime -> [Text] -> m ()
createMissing merchantId bapId now ids = do
  existing <- findAllByMerchantAndOrderIds merchantId ids
  let seen = HS.fromList (map (.orderId) existing)
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
          createdAt = now,
          updatedAt = now
        }
      | orderId <- nub ids,
        not (HS.member orderId seen)
    ]

-- Dashboard paging: orders first claimed in the range, oldest first, one DB page.
findPageByMerchantAndCreatedAtRange :: (BeamFlow m r) => Text -> UTCTime -> UTCTime -> Maybe Text -> Int -> Int -> m [Domain.RsfOrderState]
findPageByMerchantAndCreatedAtRange merchantId from to mbBapId limit offset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq from,
          Se.Is Beam.createdAt $ Se.LessThan to
        ]
          <> [Se.Is Beam.bapId $ Se.Eq (Just b) | Just b <- [mbBapId]]
    ]
    (Se.Asc Beam.createdAt)
    (Just limit)
    (Just offset)

-- Everything first seen in the range (the day summary folds the whole day).
findAllByMerchantAndCreatedAtRange :: (BeamFlow m r) => Text -> UTCTime -> UTCTime -> m [Domain.RsfOrderState]
findAllByMerchantAndCreatedAtRange merchantId from to =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq from,
          Se.Is Beam.createdAt $ Se.LessThan to
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
            createdAt = reportedAt,
            updatedAt = reportedAt
          }
