{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RsfUtrState where

import qualified Domain.Types.RsfUtrState
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RsfUtrState as Beam

instance FromTType' Beam.RsfUtrState Domain.Types.RsfUtrState.RsfUtrState where
  fromTType' (Beam.RsfUtrStateT {..}) = do
    pure $
      Just
        Domain.Types.RsfUtrState.RsfUtrState
          { bapId = bapId,
            createdAt = createdAt,
            lastClaimedAt = lastClaimedAt,
            lastReportedAt = lastReportedAt,
            lastReportedMessageId = lastReportedMessageId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            reportedDiff = reportedDiff,
            reportedStatus = reportedStatus,
            updatedAt = updatedAt,
            utr = utr
          }

instance ToTType' Beam.RsfUtrState Domain.Types.RsfUtrState.RsfUtrState where
  toTType' (Domain.Types.RsfUtrState.RsfUtrState {..}) = do
    Beam.RsfUtrStateT
      { Beam.bapId = bapId,
        Beam.createdAt = createdAt,
        Beam.lastClaimedAt = lastClaimedAt,
        Beam.lastReportedAt = lastReportedAt,
        Beam.lastReportedMessageId = lastReportedMessageId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.reportedDiff = reportedDiff,
        Beam.reportedStatus = reportedStatus,
        Beam.updatedAt = updatedAt,
        Beam.utr = utr
      }
