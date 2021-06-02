{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Types.Storage.Leads where

import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import Data.Swagger
import Data.Time
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)

data LeadsT f = Leads
  { id :: B.C f (Id Leads),
    customerId :: B.C f Text,
    fromLocationId :: B.C f (Maybe Text),
    toLocationId :: B.C f (Maybe Text),
    vehicleVariant :: B.C f (Maybe Text),
    tripDate :: B.C f Text,
    tripTime :: B.C f Text,
    noOfPassengers :: B.C f Int,
    luggageCount :: B.C f (Maybe Int),
    expiryTime :: B.C f Text,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Leads = LeadsT Identity

type LeadsPrimaryKey = B.PrimaryKey LeadsT Identity

instance B.Table LeadsT where
  data PrimaryKey LeadsT f = LeadsPrimaryKey (B.C f (Id Leads))
    deriving (Generic, B.Beamable)
  primaryKey = LeadsPrimaryKey . id

deriving instance Show Leads

deriving instance Eq Leads

instance ToJSON Leads where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Leads where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema Leads

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity LeadsT)
fieldEMod =
  B.setEntityName "booking_reference"
    <> B.modifyTableFields
      B.tableModification
        { createdAt = "created_at",
          updatedAt = "updated_at",
          customerId = "customer_id",
          fromLocationId = "from_location_id",
          toLocationId = "to_location_id",
          vehicleVariant = "vehicle_variant",
          tripDate = "trip_date",
          tripTime = "trip_time",
          noOfPassengers = "no_of_passengers",
          luggageCount = "luggage_count",
          expiryTime = "expiry_time"
        }
