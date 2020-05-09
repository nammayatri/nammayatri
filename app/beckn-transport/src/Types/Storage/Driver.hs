{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Driver where

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

data Gender = MALE | FEMALE | OTHER
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Gender where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL Gender where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Gender
instance FromHttpApiData Gender where
  parseUrlPiece  = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict


data DriverT f =
  Driver
    { _id                 :: B.C f DriverId
    , _name               :: B.C f Text
    , _mobileNumber       :: B.C f Text
    , _gender             :: B.C f Gender
    , _experience         :: B.C f Text
    , _rating             :: B.C f Text
    , _noOfTrips          :: B.C f Int
    , _description        :: B.C f Text
    , _createdAt          :: B.C f LocalTime
    , _updatedAt          :: B.C f LocalTime
    }

  deriving (Generic, B.Beamable)

type Driver = DriverT Identity

type DriverPrimaryKey = B.PrimaryKey DriverT Identity

instance B.Table DriverT where
  data PrimaryKey DriverT f = DriverPrimaryKey (B.C f DriverId)
                               deriving (Generic, B.Beamable)
  primaryKey = DriverPrimaryKey . _id

deriving instance Show Driver

deriving instance Eq Driver

instance ToJSON Driver where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Driver where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Driver

insertExpression org = insertExpressions [org]

insertExpressions orgs = B.insertValues orgs


fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _createdAt = "created_at"
      , _updatedAt = "updated_at"
      , _mobileNumber = "mobile_number"
      , _noOfTrips = "no_of_trips"
      }
