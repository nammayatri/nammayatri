{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RsfUtrStateExtra
  ( findAllByMerchantAndUtrs,
    createMissing,
    findPageByMerchantAndLastClaimedAtRange,
    findAllByMerchantAndLastClaimedAtRange,
    upsertReported,
  )
where

import qualified Data.HashSet as HS
import Data.List (nub, partition)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RsfUtrState as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.RsfUtrState as Beam
import Storage.Queries.OrphanInstances.RsfUtrState ()

-- Phase 2 (stage C, check 5): a row with reportedStatus set means the UTR is closed.
findAllByMerchantAndUtrs ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DM.Merchant ->
  [Text] ->
  m [Domain.RsfUtrState]
findAllByMerchantAndUtrs _ [] = pure []
findAllByMerchantAndUtrs merchantId utrs =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId.getId,
          Se.Is Beam.utr $ Se.In utrs
        ]
    ]

-- Phase 2: one row per UTR the first time a receiver_recon names it (reportedStatus NULL), and
-- lastClaimedAt bumped on every later receiver_recon that names it again, so the dashboard lists the
-- UTR under the day of its latest claim. One batched read, inserts for the missing ids, one update
-- for the rest.
createMissing :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> UTCTime -> [Text] -> m ()
createMissing merchantId merchantOperatingCityId bapId now ids = do
  existing <- findAllByMerchantAndUtrs merchantId ids
  let seen = HS.fromList (map (.utr) existing)
      (known, fresh) = partition (`HS.member` seen) (nub ids)
  unless (null known) $
    updateWithKV
      [Se.Set Beam.lastClaimedAt (Just now), Se.Set Beam.updatedAt now]
      [Se.And [Se.Is Beam.merchantId $ Se.Eq merchantId.getId, Se.Is Beam.utr $ Se.In known]]
  traverse_
    createWithKV
    [ Domain.RsfUtrState
        { merchantId = merchantId,
          merchantOperatingCityId = merchantOperatingCityId,
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
findPageByMerchantAndLastClaimedAtRange :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DM.Merchant -> UTCTime -> UTCTime -> Maybe Text -> Int -> Int -> m [Domain.RsfUtrState]
findPageByMerchantAndLastClaimedAtRange merchantId from to mbBapId limit offset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq merchantId.getId,
          Se.Is Beam.lastClaimedAt $ Se.GreaterThanOrEq (Just from),
          Se.Is Beam.lastClaimedAt $ Se.LessThan (Just to)
        ]
          <> [Se.Is Beam.bapId $ Se.Eq (Just b) | Just b <- [mbBapId]]
    ]
    (Se.Asc Beam.lastClaimedAt)
    (Just limit)
    (Just offset)

-- Every UTR whose latest claim falls in the range (the day summary folds the whole day), optionally one
-- collector's: with a bapId the bap_id secondary key serves the rows from KV, so fresh claims show before they drain.
findAllByMerchantAndLastClaimedAtRange :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DM.Merchant -> UTCTime -> UTCTime -> Maybe Text -> m [Domain.RsfUtrState]
findAllByMerchantAndLastClaimedAtRange merchantId from to mbBapId =
  findAllWithKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq merchantId.getId,
          Se.Is Beam.lastClaimedAt $ Se.GreaterThanOrEq (Just from),
          Se.Is Beam.lastClaimedAt $ Se.LessThan (Just to)
        ]
          <> [Se.Is Beam.bapId $ Se.Eq (Just b) | Just b <- [mbBapId]]
    ]

-- Phase 4 (Mapping D): stamped once per send, only after the collector ACKs.
upsertReported ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  HighPrecMoney ->
  Text ->
  UTCTime ->
  m ()
upsertReported merchantId merchantOperatingCityId utr reportedStatus reportedDiff messageId reportedAt = do
  let key =
        [ Se.And
            [ Se.Is Beam.merchantId $ Se.Eq merchantId.getId,
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
            merchantOperatingCityId = merchantOperatingCityId,
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
