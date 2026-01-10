{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PTCircuitBreakerHistory where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PTCircuitBreakerHistory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PTCircuitBreakerHistoryT f = PTCircuitBreakerHistoryT
  { apiType :: (B.C f Domain.Types.PTCircuitBreakerHistory.APIType),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    failureCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    newState :: (B.C f Domain.Types.PTCircuitBreakerHistory.CircuitState),
    previousState :: (B.C f Domain.Types.PTCircuitBreakerHistory.CircuitState),
    ptMode :: (B.C f Domain.Types.PTCircuitBreakerHistory.PTMode),
    reason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PTCircuitBreakerHistoryT where
  data PrimaryKey PTCircuitBreakerHistoryT f = PTCircuitBreakerHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PTCircuitBreakerHistoryId . id

type PTCircuitBreakerHistory = PTCircuitBreakerHistoryT Identity

$(enableKVPG (''PTCircuitBreakerHistoryT) [('id)] [])

$(mkTableInstances (''PTCircuitBreakerHistoryT) "pt_circuit_breaker_history")
