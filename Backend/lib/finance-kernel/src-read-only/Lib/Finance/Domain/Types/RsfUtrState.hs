{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.RsfUtrState where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data RsfUtrState = RsfUtrState
  { createdAt :: Kernel.Prelude.UTCTime,
    lastReportedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastReportedMessageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    reportedDiff :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    reportedStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    utr :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
