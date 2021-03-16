{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.FarePolicy where

import Beckn.Types.Id (Id)
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time (TimeOfDay, UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude
import qualified Types.Domain.FarePolicy as D

data FarePolicyT f = FarePolicy
  { _id :: B.C f (Id D.FarePolicy),
    _vehicleVariant :: B.C f Vehicle.Variant,
    _organizationId :: B.C f (Id Organization.Organization),
    _baseFare :: B.C f (Maybe Double),
    _baseDistance :: B.C f (Maybe Double),
    _perExtraKmRate :: B.C f Double,
    _nightShiftStart :: B.C f (Maybe TimeOfDay),
    _nightShiftEnd :: B.C f (Maybe TimeOfDay),
    _nightShiftRate :: B.C f (Maybe Double),
    _createdAt :: B.C f UTCTime,
    _updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type FarePolicy = FarePolicyT Identity

type FarePolicyPrimaryKey = B.PrimaryKey FarePolicyT Identity

instance B.Table FarePolicyT where
  data PrimaryKey FarePolicyT f = FarePolicyPrimaryKey (B.C f (Id D.FarePolicy))
    deriving (Generic, B.Beamable)
  primaryKey = FarePolicyPrimaryKey . _id

deriving instance Show FarePolicy

deriving instance Eq FarePolicy

instance ToJSON FarePolicy where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON FarePolicy where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity FarePolicyT)
fieldEMod =
  B.setEntityName "fare_policy"
    <> B.modifyTableFields
      B.tableModification
        { _vehicleVariant = "vehicle_variant",
          _organizationId = "organization_id",
          _baseFare = "base_fare",
          _baseDistance = "base_distance",
          _perExtraKmRate = "per_extra_km_rate",
          _nightShiftStart = "night_shift_start",
          _nightShiftEnd = "night_shift_end",
          _nightShiftRate = "night_shift_rate",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }

fromTable :: FarePolicy -> D.FarePolicy
fromTable sFarePolicy =
  D.FarePolicy
    { id = sFarePolicy ^. #_id,
      vehicleVariant = sFarePolicy ^. #_vehicleVariant,
      organizationId = sFarePolicy ^. #_organizationId,
      baseFare = toRational <$> sFarePolicy ^. #_baseFare,
      baseDistance = toRational <$> sFarePolicy ^. #_baseDistance,
      perExtraKmRate = toRational $ sFarePolicy ^. #_perExtraKmRate,
      nightShiftStart = sFarePolicy ^. #_nightShiftStart,
      nightShiftEnd = sFarePolicy ^. #_nightShiftEnd,
      nightShiftRate = toRational <$> sFarePolicy ^. #_nightShiftRate
    }
