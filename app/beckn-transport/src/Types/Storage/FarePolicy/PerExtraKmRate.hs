{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.FarePolicy.PerExtraKmRate where

import Beckn.Types.Id (Id)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Vehicle as Vehicle

data FarePolicyPerExtraKmRateT f = FarePolicyPerExtraKmRate
  { id :: B.C f (Id FarePolicyPerExtraKmRateT),
    vehicleVariant :: B.C f Vehicle.Variant,
    organizationId :: B.C f (Id Organization.Organization),
    extraDistanceRangeStart :: B.C f Double,
    extraFare :: B.C f Double
  }
  deriving (Generic, B.Beamable)

type FarePolicyPerExtraKmRate = FarePolicyPerExtraKmRateT Identity

type ExtraKmRatePrimaryKey = B.PrimaryKey FarePolicyPerExtraKmRateT Identity

instance B.Table FarePolicyPerExtraKmRateT where
  data PrimaryKey FarePolicyPerExtraKmRateT f = ExtraKmRatePrimaryKey (B.C f (Id FarePolicyPerExtraKmRateT))
    deriving (Generic, B.Beamable)
  primaryKey = ExtraKmRatePrimaryKey . id

deriving instance Show FarePolicyPerExtraKmRate

deriving instance Eq FarePolicyPerExtraKmRate

deriving instance FromJSON FarePolicyPerExtraKmRate

deriving instance ToJSON FarePolicyPerExtraKmRate

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity FarePolicyPerExtraKmRateT)
fieldEMod =
  B.setEntityName "fare_policy_per_extra_km_rate"
    <> B.modifyTableFields
      B.tableModification
        { vehicleVariant = "vehicle_variant",
          organizationId = "organization_id",
          extraDistanceRangeStart = "extra_distance_range_start",
          extraFare = "extra_fare"
        }