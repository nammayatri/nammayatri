{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MultiModalFareLegRules where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MultiModalFareLegRules
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data MultiModalFareLegRulesT f = MultiModalFareLegRulesT
  { amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    currency :: (B.C f Kernel.Types.Common.Currency),
    fromTimeFrameId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    maxDist :: (B.C f Kernel.Types.Common.Meters),
    minDist :: (B.C f Kernel.Types.Common.Meters),
    networkId :: (B.C f Kernel.Prelude.Text),
    passengerType :: (B.C f Domain.Types.MultiModalFareLegRules.MultiModalPassengerType),
    paymentMedia :: (B.C f Domain.Types.MultiModalFareLegRules.MultiModalFareMediaType),
    toTimeFrameId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table MultiModalFareLegRulesT where
  data PrimaryKey MultiModalFareLegRulesT f = MultiModalFareLegRulesId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MultiModalFareLegRulesId . id

type MultiModalFareLegRules = MultiModalFareLegRulesT Identity

$(enableKVPG (''MultiModalFareLegRulesT) [('id)] [])

$(mkTableInstances (''MultiModalFareLegRulesT) "multi_modal_fare_leg_rules")
