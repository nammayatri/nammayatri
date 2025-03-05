{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ServicePeopleCategory where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.ServicePeopleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.TimeBound
import Tools.Beam.UtilsTH

data ServicePeopleCategoryT f = ServicePeopleCategoryT
  { cancellationCharges :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    description :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    pricePerUnit :: B.C f Kernel.Types.Common.HighPrecMoney,
    pricingType :: B.C f (Kernel.Prelude.Maybe Domain.Types.ServicePeopleCategory.PricingType),
    timeBounds :: B.C f (Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound),
    vendorSplitDetails :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ServicePeopleCategoryT where
  data PrimaryKey ServicePeopleCategoryT f = ServicePeopleCategoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ServicePeopleCategoryId . id

type ServicePeopleCategory = ServicePeopleCategoryT Identity

$(enableKVPG ''ServicePeopleCategoryT ['id] [])

$(mkTableInstances ''ServicePeopleCategoryT "service_people_category")
