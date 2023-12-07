{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ServicePeopleCategory where

import qualified Database.Beam as B
import qualified Domain.Types.ServicePeopleCategory
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data ServicePeopleCategoryT f = ServicePeopleCategoryT
  { description :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    pricePerUnit :: B.C f Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table ServicePeopleCategoryT where
  data PrimaryKey ServicePeopleCategoryT f = ServicePeopleCategoryId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = ServicePeopleCategoryId . id

type ServicePeopleCategory = ServicePeopleCategoryT Identity

$(enableKVPG ''ServicePeopleCategoryT ['id] [])

$(mkTableInstances ''ServicePeopleCategoryT "service_people_category")
