{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.PgPaymentSettlementReportExtra where

import qualified Data.Time as Time
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.PgPaymentSettlementReport as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.PgPaymentSettlementReport
import qualified Sequelize as Se

findByReferenceIds ::
  (BeamFlow m r) =>
  [Text] -> -- referenceIds
  m [Domain.PgPaymentSettlementReport]
findByReferenceIds referenceIds =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.referenceId $ Se.In (map Just referenceIds),
          Se.Is Beam.txnStatus $ Se.Eq Domain.SUCCESS
        ]
    ]

findByTxnDateRangeAndStatus ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  UTCTime -> -- startTime
  UTCTime -> -- endTime
  m [Domain.PgPaymentSettlementReport]
findByTxnDateRangeAndStatus merchantId merchantOpCityId startTime endTime = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId,
          Se.Is Beam.txnStatus $ Se.Eq Domain.SUCCESS,
          Se.Is Beam.txnDate $ Se.GreaterThanOrEq (Just startTime),
          Se.Is Beam.txnDate $ Se.LessThanOrEq (Just endTime)
        ]
    ]
