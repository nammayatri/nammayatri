{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.RsfUtrStateExtra
  ( findAllByMerchantAndUtrs,
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

-- Phase 2: one row per UTR the first time a receiver_recon names it (reportedStatus NULL), and
-- lastClaimedAt bumped on every later receiver_recon that names it again, so the dashboard lists the
-- UTR under the day of its latest claim. One batched read, inserts for the missing ids, one update
-- for the rest.
createMissing :: (BeamFlow m r) => Text -> Maybe Text -> UTCTime -> [Text] -> m ()
createMissing merchantId bapId now ids = do
  existing <- findAllByMerchantAndUtrs merchantId ids
  let seen = HS.fromList (map (.utr) existing)
      (known, fresh) = partition (`HS.member` seen) (nub ids)
  unless (null known) $
    updateWithKV
      [Se.Set Beam.lastClaimedAt (Just now), Se.Set Beam.updatedAt now]
      [Se.And [Se.Is Beam.merchantId $ Se.Eq merchantId, Se.Is Beam.utr $ Se.In known]]
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
          lastClaimedAt = Just now,
          createdAt = now,
          updatedAt = now
        }
      | utr <- fresh
    ]

-- Dashboard paging: UTRs whose latest claim falls in the range, oldest first, one DB page.
findPageByMerchantAndLastClaimedAtRange :: (BeamFlow m r) => Text -> UTCTime -> UTCTime -> Maybe Text -> Int -> Int -> m [Domain.RsfUtrState]
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

-- Every UTR whose latest claim falls in the range (the day summary folds the whole day).
findAllByMerchantAndLastClaimedAtRange :: (BeamFlow m r) => Text -> UTCTime -> UTCTime -> m [Domain.RsfUtrState]
findAllByMerchantAndLastClaimedAtRange merchantId from to =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.lastClaimedAt $ Se.GreaterThanOrEq (Just from),
          Se.Is Beam.lastClaimedAt $ Se.LessThan (Just to)
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
            lastClaimedAt = Just reportedAt,
            createdAt = reportedAt,
            updatedAt = reportedAt
          }
