{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.FarePolicy.Discount where

import Beckn.Types.Id (Id)
import Data.Time (TimeOfDay, UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Vehicle as Vehicle

data FarePolicyDiscountT f = FarePolicyDiscount
  { id :: B.C f (Id FarePolicyDiscount),
    vehicleVariant :: B.C f Vehicle.Variant,
    organizationId :: B.C f (Id Organization.Organization),
    startTime :: B.C f TimeOfDay,
    endTime :: B.C f TimeOfDay,
    enabled :: B.C f Bool,
    discount :: B.C f Double,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type FarePolicyDiscount = FarePolicyDiscountT Identity

type FarePolicyDiscountPrimaryKey = B.PrimaryKey FarePolicyDiscountT Identity

instance B.Table FarePolicyDiscountT where
  data PrimaryKey FarePolicyDiscountT f = FarePolicyDiscountPrimaryKey (B.C f (Id FarePolicyDiscount))
    deriving (Generic, B.Beamable)
  primaryKey = FarePolicyDiscountPrimaryKey . id

deriving instance Show FarePolicyDiscount

deriving instance Eq FarePolicyDiscount

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity FarePolicyDiscountT)
fieldEMod =
  B.setEntityName "fare_policy_discount"
    <> B.modifyTableFields
      B.tableModification
        { vehicleVariant = "vehicle_variant",
          organizationId = "organization_id",
          startTime = "start_time",
          endTime = "end_time",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }
