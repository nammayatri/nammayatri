{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PenaltyRule where

import qualified Database.Beam as B
import qualified Domain.Types.PenaltyRule
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PenaltyRuleT f = PenaltyRuleT
  { id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    triggerEvent :: B.C f Domain.Types.PenaltyRule.PenaltyTriggerEvent,
    conditionsJson :: B.C f Kernel.Prelude.Text,
    penaltyType :: B.C f Domain.Types.PenaltyRule.PenaltyAmountType,
    fixedAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    percentage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    formulaExpression :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    gracePeriodCount :: B.C f Kernel.Prelude.Int,
    gracePeriodWindowHours :: B.C f Kernel.Prelude.Int,
    priority :: B.C f Kernel.Prelude.Int,
    isActive :: B.C f Kernel.Prelude.Bool,
    startDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    endDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PenaltyRuleT where
  data PrimaryKey PenaltyRuleT f = PenaltyRuleId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PenaltyRuleId . id

type PenaltyRule = PenaltyRuleT Identity

$(enableKVPG ''PenaltyRuleT ['id] [['merchantId]])

$(mkTableInstances ''PenaltyRuleT "penalty_rule")
