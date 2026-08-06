{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.RsfUtrStateExtra
  ( findAllByMerchantAndUtrs,
    upsertReported,
  )
where

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
            reportedStatus = Just reportedStatus,
            reportedDiff = Just reportedDiff,
            lastReportedMessageId = Just messageId,
            lastReportedAt = Just reportedAt,
            createdAt = reportedAt,
            updatedAt = reportedAt
          }
