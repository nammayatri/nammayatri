{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.FarePolicy where

import Beckn.Types.Id (Id)
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Beckn.Utils.JSON
import Data.Time (TimeOfDay, UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Domain.FarePolicy as D

data FarePolicyT f = FarePolicy
  { id :: B.C f (Id D.FarePolicy),
    vehicleVariant :: B.C f Vehicle.Variant,
    organizationId :: B.C f (Id Organization.Organization),
    baseFare :: B.C f (Maybe Double),
    baseDistance :: B.C f (Maybe Double),
    perExtraKmRate :: B.C f Double,
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
  data PrimaryKey FarePolicyT f = FarePolicyPrimaryKey (B.C f (Id D.FarePolicy))
    deriving (Generic, B.Beamable)
  primaryKey = FarePolicyPrimaryKey . id

deriving instance Show FarePolicy

deriving instance Eq FarePolicy

instance ToJSON FarePolicy where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON FarePolicy where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity FarePolicyT)
fieldEMod =
  B.setEntityName "fare_policy"
    <> B.modifyTableFields
      B.tableModification
        { vehicleVariant = "vehicle_variant",
          organizationId = "organization_id",
          baseFare = "base_fare",
          baseDistance = "base_distance",
          perExtraKmRate = "per_extra_km_rate",
          nightShiftStart = "night_shift_start",
          nightShiftEnd = "night_shift_end",
          nightShiftRate = "night_shift_rate",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }

fromTable :: FarePolicy -> D.FarePolicy
fromTable sFarePolicy =
  D.FarePolicy
    { id = sFarePolicy.id,
      vehicleVariant = sFarePolicy.vehicleVariant,
      organizationId = sFarePolicy.organizationId,
      baseFare = toRational <$> sFarePolicy.baseFare,
      baseDistance = toRational <$> sFarePolicy.baseDistance,
      perExtraKmRate = toRational $ sFarePolicy.perExtraKmRate,
      nightShiftStart = sFarePolicy.nightShiftStart,
      nightShiftEnd = sFarePolicy.nightShiftEnd,
      nightShiftRate = toRational <$> sFarePolicy.nightShiftRate
    }
