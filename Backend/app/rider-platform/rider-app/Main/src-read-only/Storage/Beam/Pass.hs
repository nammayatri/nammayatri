{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Pass where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Pass
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PassT f = PassT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    benefit :: B.C f (Kernel.Prelude.Maybe Domain.Types.Pass.Benefit),
    code :: B.C f Kernel.Prelude.Text,
    days :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    order :: B.C f Kernel.Prelude.Int,
    passTypeId :: B.C f Kernel.Prelude.Text,
    purchaseEligibilityJsonLogic :: B.C f [Kernel.Prelude.Text],
    redeemEligibilityJsonLogic :: B.C f [Kernel.Prelude.Text],
    savings :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    trips :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    vehicleServiceTierType :: B.C f BecknV2.FRFS.Enums.ServiceTierType,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PassT where
  data PrimaryKey PassT f = PassId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PassId . id

type Pass = PassT Identity

$(enableKVPG ''PassT ['id] [['passTypeId]])

$(mkTableInstances ''PassT "pass")
