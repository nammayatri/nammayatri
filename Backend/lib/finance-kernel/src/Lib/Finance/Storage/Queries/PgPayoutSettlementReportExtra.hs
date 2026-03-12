{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.PgPayoutSettlementReportExtra where

import qualified Data.Time as Time
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.PgPayoutSettlementReport as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.PgPayoutSettlementReport as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.PgPayoutSettlementReport
import qualified Sequelize as Se

findByPayoutRequestIds ::
  (BeamFlow m r) =>
  [Text] -> -- payoutRequestIds
  m [Domain.PgPayoutSettlementReport]
findByPayoutRequestIds payoutRequestIds =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.payoutRequestId $ Se.In (map Just payoutRequestIds),
          Se.Is Beam.txnStatus $ Se.Eq Domain.SUCCESS
        ]
    ]

findByTxnDateRangeAndStatus ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  UTCTime -> -- startTime
  UTCTime -> -- endTime
  m [Domain.PgPayoutSettlementReport]
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
