{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RsfOrderStateExtra
  ( findAllByMerchantAndOrderIds,
    createMissing,
    findPageByMerchantAndLastClaimedAtRange,
    findAllByMerchantAndLastClaimedAtRange,
    upsertReported,
  )
where

import qualified Data.HashSet as HS
import Data.List (partition)
import qualified Data.Map.Strict as Map
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RsfOrderState as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.RsfOrderState as Beam
import Storage.Queries.OrphanInstances.RsfOrderState ()

findAllByMerchantAndOrderIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DM.Merchant ->
  [Text] ->
  m [Domain.RsfOrderState]
findAllByMerchantAndOrderIds _ [] = pure []
findAllByMerchantAndOrderIds merchantId orderIds =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId.getId,
          Se.Is Beam.orderId $ Se.In orderIds
        ]
    ]

-- Phase 2: one row per order the first time a receiver_recon names it (reportedStatus NULL), and
-- lastClaimedAt bumped on every later receiver_recon that names it again, so the dashboard lists the
-- order under the day of its latest claim. One batched read, inserts for the missing ids, one update
-- for the rest. Each order comes with its city (its ride's).
createMissing :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DM.Merchant -> Maybe Text -> UTCTime -> [(Text, Id DMOC.MerchantOperatingCity)] -> m ()
createMissing merchantId bapId now orderCities = do
  let cityByOrder = Map.fromList orderCities
      ids = Map.keys cityByOrder
  existing <- findAllByMerchantAndOrderIds merchantId ids
  let seen = HS.fromList (map (.orderId) existing)
      (known, fresh) = partition (`HS.member` seen) ids
  unless (null known) $
    updateWithKV
      [Se.Set Beam.lastClaimedAt (Just now), Se.Set Beam.updatedAt now]
      [Se.And [Se.Is Beam.merchantId $ Se.Eq merchantId.getId, Se.Is Beam.orderId $ Se.In known]]
  traverse_
    createWithKV
    [ Domain.RsfOrderState
        { merchantId = merchantId,
          merchantOperatingCityId = city,
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
      | orderId <- fresh,
        Just city <- [Map.lookup orderId cityByOrder]
    ]

-- Dashboard paging: orders whose latest claim falls in the range, oldest first, one DB page.
findPageByMerchantAndLastClaimedAtRange :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DM.Merchant -> UTCTime -> UTCTime -> Maybe Text -> Int -> Int -> m [Domain.RsfOrderState]
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

-- Every order whose latest claim falls in the range (the day summary folds the whole day), optionally one
-- collector's: with a bapId the bap_id secondary key serves the rows from KV, so fresh claims show before they drain.
findAllByMerchantAndLastClaimedAtRange :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DM.Merchant -> UTCTime -> UTCTime -> Maybe Text -> m [Domain.RsfOrderState]
findAllByMerchantAndLastClaimedAtRange merchantId from to mbBapId =
  findAllWithKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq merchantId.getId,
          Se.Is Beam.lastClaimedAt $ Se.GreaterThanOrEq (Just from),
          Se.Is Beam.lastClaimedAt $ Se.LessThan (Just to)
        ]
          <> [Se.Is Beam.bapId $ Se.Eq (Just b) | Just b <- [mbBapId]]
    ]

-- Phase 4 (Mapping C): stamped once per send, only after the collector ACKs.
upsertReported ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  HighPrecMoney ->
  Maybe Text ->
  Text ->
  UTCTime ->
  m ()
upsertReported merchantId merchantOperatingCityId orderId reportedStatus reportedDiff reportedCode messageId reportedAt = do
  let key =
        [ Se.And
            [ Se.Is Beam.merchantId $ Se.Eq merchantId.getId,
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
            merchantOperatingCityId = merchantOperatingCityId,
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
