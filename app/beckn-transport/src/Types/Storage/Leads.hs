{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Leads where

import Data.Aeson
import Data.Swagger
import Data.Time.LocalTime
import qualified Database.Beam as B
import EulerHS.Prelude
import Types.App

data LeadsT f = Leads
  { _id :: B.C f LeadsId,
    _customerId :: B.C f Text,
    _fromLocationId :: B.C f (Maybe Text),
    _toLocationId :: B.C f (Maybe Text),
    _vehicleVariant :: B.C f (Maybe Text),
    _tripDate :: B.C f Text,
    _tripTime :: B.C f Text,
    _noOfPassengers :: B.C f Int,
    _luggageCount :: B.C f (Maybe Int),
    _expiryTime :: B.C f Text,
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

type Leads = LeadsT Identity

type LeadsPrimaryKey = B.PrimaryKey LeadsT Identity

instance B.Table LeadsT where
  data PrimaryKey LeadsT f = LeadsPrimaryKey (B.C f LeadsId)
    deriving (Generic, B.Beamable)
  primaryKey = LeadsPrimaryKey . _id

deriving instance Show Leads

deriving instance Eq Leads

instance ToJSON Leads where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Leads where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Leads

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity LeadsT)
fieldEMod =
  B.setEntityName "booking_reference"
    <> B.modifyTableFields
      B.tableModification
        { _createdAt = "created_at",
          _updatedAt = "updated_at",
          _customerId = "customer_id",
          _fromLocationId = "from_location_id",
          _toLocationId = "to_location_id",
          _vehicleVariant = "vehicle_variant",
          _tripDate = "trip_date",
          _tripTime = "trip_time",
          _noOfPassengers = "no_of_passengers",
          _luggageCount = "luggage_count",
          _expiryTime = "expiry_time"
        }
