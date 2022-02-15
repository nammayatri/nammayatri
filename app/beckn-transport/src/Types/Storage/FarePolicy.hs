{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.FarePolicy where

import Beckn.Types.Id (Id)
import Data.Time (TimeOfDay, UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Storage.FarePolicy.Discount
import Types.Storage.FarePolicy.PerExtraKmRate
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Vehicle as Vehicle

data FarePolicyT f = FarePolicy
  { id :: B.C f (Id FarePolicy),
    vehicleVariant :: B.C f Vehicle.Variant,
    organizationId :: B.C f (Id Organization.Organization),
    baseFare :: B.C f (Maybe Double),
    nightShiftStart :: B.C f (Maybe TimeOfDay),
    nightShiftEnd :: B.C f (Maybe TimeOfDay),
    nightShiftRate :: B.C f (Maybe Double),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type FarePolicy = FarePolicyT Identity

type FarePolicyPrimaryKey = B.PrimaryKey FarePolicyT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table FarePolicyT where
  data PrimaryKey FarePolicyT f = FarePolicyPrimaryKey (B.C f (Id FarePolicy))
    deriving (Generic, B.Beamable)
  primaryKey t = FarePolicyPrimaryKey t.id

deriving instance Show FarePolicy

deriving instance Eq FarePolicy

deriving instance FromJSON FarePolicy

deriving instance ToJSON FarePolicy

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity FarePolicyT)
fieldEMod =
  B.setEntityName "fare_policy"
    <> B.modifyTableFields
      B.tableModification
        { vehicleVariant = "vehicle_variant",
          organizationId = "organization_id",
          baseFare = "base_fare",
          nightShiftStart = "night_shift_start",
          nightShiftEnd = "night_shift_end",
          nightShiftRate = "night_shift_rate",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }

data FarePolicyData = FarePolicyData
  { farePolicy :: FarePolicy,
    extraKmRateList :: [FarePolicyPerExtraKmRate],
    discountList :: [FarePolicyDiscount]
  }
