{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Pass where

import qualified BecknV2.FRFS.Enums
import qualified Data.Aeson
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
    applicableVehicleServiceTiers :: B.C f [BecknV2.FRFS.Enums.ServiceTierType],
    autoApply :: B.C f Kernel.Prelude.Bool,
    benefit :: B.C f (Kernel.Prelude.Maybe Domain.Types.Pass.Benefit),
    benefitDescription :: B.C f Kernel.Prelude.Text,
    code :: B.C f Kernel.Prelude.Text,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    documentsRequired :: B.C f [Domain.Types.Pass.PassDocumentType],
    enable :: B.C f Kernel.Prelude.Bool,
    id :: B.C f Kernel.Prelude.Text,
    maxAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    maxValidDays :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    maxValidTrips :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    minAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    name :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    order :: B.C f Kernel.Prelude.Int,
    passTypeCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    passTypeId :: B.C f Kernel.Prelude.Text,
    purchaseEligibilityJsonLogic :: B.C f [Data.Aeson.Value],
    redeemEligibilityJsonLogic :: B.C f [Data.Aeson.Value],
    verificationValidity :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
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
