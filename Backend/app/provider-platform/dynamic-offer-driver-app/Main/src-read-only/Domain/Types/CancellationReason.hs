{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CancellationReason where

import Data.Aeson
import qualified Data.Text
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH

data CancellationReason = CancellationReason
  { description :: Data.Text.Text,
    enabled :: Kernel.Prelude.Bool,
    priority :: Kernel.Prelude.Int,
    reasonCode :: Domain.Types.CancellationReason.CancellationReasonCode,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype CancellationReasonCode = CancellationReasonCode Data.Text.Text deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CancellationReasonCode)
