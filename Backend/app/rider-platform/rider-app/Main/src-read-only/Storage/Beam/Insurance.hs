{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Insurance where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.Insurance
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data InsuranceT f = InsuranceT
  { category :: B.C f Kernel.Prelude.Text,
    certificateUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    customerId :: B.C f Kernel.Prelude.Text,
    customerName :: B.C f Kernel.Prelude.Text,
    customerPhone :: B.C f Kernel.Prelude.Text,
    driverInsuredAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverPhone :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    endDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    entityType :: B.C f Domain.Types.Insurance.EntityType,
    fairBreakup :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    id :: B.C f Kernel.Prelude.Text,
    insuredAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    partnerId :: B.C f Kernel.Prelude.Text,
    plan :: B.C f Kernel.Prelude.Text,
    policyId :: B.C f Kernel.Prelude.Text,
    policyNumber :: B.C f Kernel.Prelude.Text,
    startDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    tripCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table InsuranceT where
  data PrimaryKey InsuranceT f = InsuranceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = InsuranceId . id

type Insurance = InsuranceT Identity

$(enableKVPG ''InsuranceT ['id] [])

$(mkTableInstances ''InsuranceT "insurance")
