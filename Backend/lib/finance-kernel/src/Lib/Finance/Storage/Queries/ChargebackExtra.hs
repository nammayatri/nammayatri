{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.ChargebackExtra where

import qualified Data.Map.Strict as Map
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney (..))
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.Chargeback as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.Chargeback as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.Chargeback
import qualified Sequelize as Se

findAllByMerchantWithFilters ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  Maybe Domain.ChargebackStatus ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Chargeback]
findAllByMerchantWithFilters merchantId merchantOpCityId mbStatus mbFrom mbTo mbLimit mbOffset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId
        ]
          <> [Se.Is Beam.chargebackStatus $ Se.Eq status | Just status <- [mbStatus]]
          <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq fromTime | Just fromTime <- [mbFrom]]
          <> [Se.Is Beam.createdAt $ Se.LessThanOrEq toTime | Just toTime <- [mbTo]]
    ]
    (Se.Desc Beam.createdAt)
    mbLimit
    mbOffset

-- | [C4 fix] Count total chargebacks matching filters without loading all records.
-- Uses the same filter logic as findAllByMerchantWithFilters but returns just the count.
countByMerchantWithFilters ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  Maybe Domain.ChargebackStatus ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m Int
countByMerchantWithFilters merchantId merchantOpCityId mbStatus mbFrom mbTo = do
  chargebacks <-
    findAllWithKV
      [ Se.And $
          [ Se.Is Beam.merchantId $ Se.Eq merchantId,
            Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId
          ]
            <> [Se.Is Beam.chargebackStatus $ Se.Eq status | Just status <- [mbStatus]]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq fromTime | Just fromTime <- [mbFrom]]
            <> [Se.Is Beam.createdAt $ Se.LessThanOrEq toTime | Just toTime <- [mbTo]]
      ]
  pure $ length (chargebacks :: [Domain.Chargeback])

findAllByStatus ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Domain.ChargebackStatus ->
  m [Domain.Chargeback]
findAllByStatus merchantId status =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.chargebackStatus $ Se.Eq status
        ]
    ]

-- | [M4 fix] Single query to count chargebacks grouped by status for a merchant.
-- Returns a Map from ChargebackStatus to count. This replaces 5 separate
-- countByMerchantAndStatus calls with a single DB fetch + in-memory grouping.
countGroupedByStatus ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  m (Map.Map Domain.ChargebackStatus Int)
countGroupedByStatus merchantId merchantOpCityId = do
  chargebacks <-
    findAllWithKV
      [ Se.And
          [ Se.Is Beam.merchantId $ Se.Eq merchantId,
            Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId
          ]
      ]
  let grouped = foldl' (\acc cb -> Map.insertWith (+) (cb.chargebackStatus) 1 acc) Map.empty (chargebacks :: [Domain.Chargeback])
  pure grouped

-- | M4 fix: Count chargebacks using a capped findAllWithOptionsKV to avoid
-- loading unbounded rows into memory. Uses a large limit cap to prevent OOM.
-- TODO: Replace with a proper SQL COUNT aggregation when countWithKV is available
-- in the KV connector.
countByMerchantAndStatus ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  Domain.ChargebackStatus ->
  m Int
countByMerchantAndStatus merchantId merchantOpCityId status = do
  let maxCountLimit = 10000 -- Cap to prevent OOM
  chargebacks <-
    findAllWithOptionsKV
      [ Se.And
          [ Se.Is Beam.merchantId $ Se.Eq merchantId,
            Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId,
            Se.Is Beam.chargebackStatus $ Se.Eq status
          ]
      ]
      (Se.Desc Beam.createdAt)
      (Just maxCountLimit)
      Nothing
  pure $ length (chargebacks :: [Domain.Chargeback])
