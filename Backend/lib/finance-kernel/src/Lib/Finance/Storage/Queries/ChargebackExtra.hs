{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.ChargebackExtra where

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

countByMerchantAndStatus ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  Domain.ChargebackStatus ->
  m Int
countByMerchantAndStatus merchantId merchantOpCityId status = do
  chargebacks <-
    findAllWithKV
      [ Se.And
          [ Se.Is Beam.merchantId $ Se.Eq merchantId,
            Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId,
            Se.Is Beam.chargebackStatus $ Se.Eq status
          ]
      ]
  pure $ length (chargebacks :: [Domain.Chargeback])
