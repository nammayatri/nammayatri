{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.RsfUtrStateExtra
  ( findAllByMerchantAndUtrs,
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
import qualified Lib.Finance.Domain.Types.RsfUtrState as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.RsfUtrState as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.RsfUtrState ()
import qualified Sequelize as Se

-- Phase 2 (stage C, check 5): a row with reportedStatus set means the UTR is closed.
findAllByMerchantAndUtrs ::
  (BeamFlow m r) =>
  Text ->
  [Text] ->
  m [Domain.RsfUtrState]
findAllByMerchantAndUtrs _ [] = pure []
findAllByMerchantAndUtrs merchantId utrs =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.utr $ Se.In utrs
        ]
    ]

-- Phase 2: one row per UTR the first time a receiver_recon mentions it, reportedStatus NULL.
-- One batched read for the ids already present, then inserts only for the missing ones.
createMissing :: (BeamFlow m r) => Text -> Maybe Text -> UTCTime -> [Text] -> m ()
createMissing merchantId bapId now ids = do
  existing <- findAllByMerchantAndUtrs merchantId ids
  let seen = HS.fromList (map (.utr) existing)
  traverse_
    createWithKV
    [ Domain.RsfUtrState
        { merchantId = merchantId,
          utr = utr,
          bapId = bapId,
          reportedStatus = Nothing,
          reportedDiff = Nothing,
          lastReportedMessageId = Nothing,
          lastReportedAt = Nothing,
          createdAt = now,
          updatedAt = now
        }
      | utr <- nub ids,
        not (HS.member utr seen)
    ]

-- Dashboard paging: UTRs first mentioned in the range, oldest first, one DB page.
findPageByMerchantAndCreatedAtRange :: (BeamFlow m r) => Text -> UTCTime -> UTCTime -> Maybe Text -> Int -> Int -> m [Domain.RsfUtrState]
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
findAllByMerchantAndCreatedAtRange :: (BeamFlow m r) => Text -> UTCTime -> UTCTime -> m [Domain.RsfUtrState]
findAllByMerchantAndCreatedAtRange merchantId from to =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq from,
          Se.Is Beam.createdAt $ Se.LessThan to
        ]
    ]

-- Phase 4 (Mapping D): stamped once per send, only after the collector ACKs.
upsertReported ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  Text ->
  HighPrecMoney ->
  Text ->
  UTCTime ->
  m ()
upsertReported merchantId utr reportedStatus reportedDiff messageId reportedAt = do
  let key =
        [ Se.And
            [ Se.Is Beam.merchantId $ Se.Eq merchantId,
              Se.Is Beam.utr $ Se.Eq utr
            ]
        ]
  existing <- findOneWithKV key
  case (existing :: Maybe Domain.RsfUtrState) of
    Just _ ->
      updateWithKV
        [ Se.Set Beam.reportedStatus (Just reportedStatus),
          Se.Set Beam.reportedDiff (Just reportedDiff),
          Se.Set Beam.lastReportedMessageId (Just messageId),
          Se.Set Beam.lastReportedAt (Just reportedAt),
          Se.Set Beam.updatedAt reportedAt
        ]
        key
    Nothing ->
      createWithKV
        Domain.RsfUtrState
          { merchantId = merchantId,
            utr = utr,
            bapId = Nothing,
            reportedStatus = Just reportedStatus,
            reportedDiff = Just reportedDiff,
            lastReportedMessageId = Just messageId,
            lastReportedAt = Just reportedAt,
            createdAt = reportedAt,
            updatedAt = reportedAt
          }
