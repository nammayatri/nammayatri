{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.FarePolicy.ExtraKmRate where

import Beckn.Types.Id (Id)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Vehicle as Vehicle

data ExtraKmRateT f = ExtraKmRate
  { id :: B.C f (Id ExtraKmRateT),
    vehicleVariant :: B.C f Vehicle.Variant,
    organizationId :: B.C f (Id Organization.Organization),
    fromExtraDistance :: B.C f Double,
    extraFare :: B.C f Double
  }
  deriving (Generic, B.Beamable)

type ExtraKmRate = ExtraKmRateT Identity

type ExtraKmRatePrimaryKey = B.PrimaryKey ExtraKmRateT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table ExtraKmRateT where
  data PrimaryKey ExtraKmRateT f = ExtraKmRatePrimaryKey (B.C f (Id ExtraKmRateT))
    deriving (Generic, B.Beamable)
  primaryKey = ExtraKmRatePrimaryKey . id

deriving instance Show ExtraKmRate

deriving instance Eq ExtraKmRate

deriving instance FromJSON ExtraKmRate

deriving instance ToJSON ExtraKmRate

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity ExtraKmRateT)
fieldEMod =
  B.setEntityName "fare_policy_extra_km_rate"
    <> B.modifyTableFields
      B.tableModification
        { vehicleVariant = "vehicle_variant",
          organizationId = "organization_id",
          fromExtraDistance = "from_extra_distance",
          extraFare = "extra_fare"
        }