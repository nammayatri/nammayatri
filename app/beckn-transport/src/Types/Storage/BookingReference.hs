{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.BookingReference where

import           Types.App
import           Data.Aeson
import qualified Data.ByteString.Lazy      as BSL
import           Data.Swagger
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as DT
import           Data.Time.LocalTime
import qualified Database.Beam             as B
import           Database.Beam.Backend.SQL
import           Database.Beam.MySQL
import           EulerHS.Prelude
import           Servant.API
import           Servant.Swagger

data BookingReferenceT f =
  BookingReference
    { _id               :: B.C f BookingReferenceId
    , _customerId       :: B.C f Text
    , _fromLocationId   :: B.C f (Maybe Text)
    , _toLocationId     :: B.C f (Maybe Text)
    , _vehicleVariant   :: B.C f (Maybe Text)
    , _tripDate         :: B.C f Text
    , _tripTime         :: B.C f Text
    , _noOfPassengers   :: B.C f Int
    , _luggageCount     :: B.C f (Maybe Int)
    , _expiryTime       :: B.C f Text
    , _createdAt    :: B.C f LocalTime
    , _updatedAt    :: B.C f LocalTime
    }
    

  deriving (Generic, B.Beamable)

type BookingReference = BookingReferenceT Identity

type BookingReferencePrimaryKey = B.PrimaryKey BookingReferenceT Identity

instance B.Table BookingReferenceT where
  data PrimaryKey BookingReferenceT f = BookingReferencePrimaryKey (B.C f BookingReferenceId)
                               deriving (Generic, B.Beamable)
  primaryKey = BookingReferencePrimaryKey . _id

deriving instance Show BookingReference

deriving instance Eq BookingReference

instance ToJSON BookingReference where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON BookingReference where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema BookingReference

insertExpression org = insertExpressions [org]

insertExpressions orgs = B.insertValues orgs


fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity BookingReferenceT)
fieldEMod =
  B.setEntityName "booking_reference" <>
    B.modifyTableFields
      B.tableModification
        { _createdAt = "created_at"
        , _updatedAt = "updated_at"
        , _customerId     = "customer_id"
        , _fromLocationId = "from_location_id"
        , _toLocationId   = "to_location_id"
        , _vehicleVariant = "vehicle_variant"
        , _tripDate       = "trip_date"
        , _tripTime       = "trip_time"
        , _noOfPassengers = "no_of_passengers"
        , _luggageCount   = "luggage_count"
        , _expiryTime     = "expiry_time"
        }
