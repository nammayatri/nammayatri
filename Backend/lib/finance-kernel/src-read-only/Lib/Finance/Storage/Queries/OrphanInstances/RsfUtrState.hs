{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.RsfUtrState where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.RsfUtrState
import qualified Lib.Finance.Storage.Beam.RsfUtrState as Beam

instance FromTType' Beam.RsfUtrState Lib.Finance.Domain.Types.RsfUtrState.RsfUtrState where
  fromTType' (Beam.RsfUtrStateT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.RsfUtrState.RsfUtrState
          { createdAt = createdAt,
            lastReportedAt = lastReportedAt,
            lastReportedMessageId = lastReportedMessageId,
            merchantId = merchantId,
            reportedDiff = reportedDiff,
            reportedStatus = reportedStatus,
            updatedAt = updatedAt,
            utr = utr
          }

instance ToTType' Beam.RsfUtrState Lib.Finance.Domain.Types.RsfUtrState.RsfUtrState where
  toTType' (Lib.Finance.Domain.Types.RsfUtrState.RsfUtrState {..}) = do
    Beam.RsfUtrStateT
      { Beam.createdAt = createdAt,
        Beam.lastReportedAt = lastReportedAt,
        Beam.lastReportedMessageId = lastReportedMessageId,
        Beam.merchantId = merchantId,
        Beam.reportedDiff = reportedDiff,
        Beam.reportedStatus = reportedStatus,
        Beam.updatedAt = updatedAt,
        Beam.utr = utr
      }
