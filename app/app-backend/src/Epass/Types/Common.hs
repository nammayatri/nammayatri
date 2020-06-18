{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Epass.Types.Common where

import Beckn.Types.Storage.Location (LocationType (..))
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.Beam.Query (HasSqlEqualityCheck)
import EulerHS.Prelude
import Servant
import Servant.Swagger
import Web.HttpApiData

data ErrorResponse = ErrorResponse
  { status :: Text,
    responseCode :: Text,
    responseMessage :: Text
  }
  deriving (Show, Generic, ToJSON, ToSchema)

data LoginMode
  = VERIFY
  | RESEND
  deriving (Generic, FromJSON, ToSchema)

data PassApplicationType
  = SELF
  | SPONSOR
  | BULKSPONSOR
  deriving (Eq, Generic, FromJSON, ToSchema)

instance Default PassApplicationType where
  def = SELF

data TravellerIDType
  = MOBILE
  | AADHAAR
  deriving (Eq, Generic, FromJSON, ToSchema)

instance Default TravellerIDType where
  def = MOBILE

data PassAction
  = REVOKE
  | EXPIRE
  deriving (Generic, FromJSON, ToSchema)

data PassIDType
  = MOBILENUMBER
  | CUSTOMERID
  | PASSAPPLICATIONID
  | ORGANIZATIONID
  deriving (Generic, FromJSON, ToSchema)

instance ToParamSchema PassIDType

instance FromHttpApiData PassIDType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

data PassType
  = INDIVIDUAL
  | ORGANIZATION
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance ToParamSchema PassType

instance HasSqlValueSyntax be String => HasSqlValueSyntax be PassType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres PassType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData PassType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

-- data LocationType
--   = POINT
--   | POLYGON
--   | PINCODE
--   deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

-- deriving instance HasSqlEqualityCheck Postgres LocationType

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be LocationType where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance FromBackendRow Postgres LocationType where
--   fromBackendRow = read . T.unpack <$> fromBackendRow

-- instance FromHttpApiData LocationType where
--   parseUrlPiece = parseHeader . DT.encodeUtf8
--   parseQueryParam = parseUrlPiece
--   parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

data Location = Location
  { _type :: LocationType,
    _lat :: Maybe Double,
    _long :: Maybe Double,
    _ward :: Maybe Text,
    _district :: Maybe Text,
    _city :: Maybe Text,
    _state :: Maybe Text,
    _country :: Maybe Text,
    _pincode :: Maybe Int,
    _address :: Maybe Text,
    _bound :: Maybe Bound
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON Location where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Location where
  toJSON = genericToJSON stripAllLensPrefixOptions

newtype Bound
  = Bound Value
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance ToSchema Bound where
  declareNamedSchema _ =
    return $ NamedSchema (Just "Bound") (sketchSchema Aeson.Null)

deriving newtype instance HasSqlValueSyntax PgValueSyntax Bound

deriving newtype instance FromBackendRow Postgres Bound

data QuotaType
  = DAILY
  | HOURLY
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be QuotaType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres QuotaType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData QuotaType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

data EntityType
  = LOCATION
  | ORG
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

deriving instance HasSqlEqualityCheck Postgres EntityType

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EntityType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres EntityType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData EntityType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

data Ack = Ack
  { _action :: Text,
    _message :: Text
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON Ack where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Ack where
  toJSON = genericToJSON stripLensPrefixOptions

data DocumentEntity
  = CUSTOMER
  | USER
  | PASSAPPLICATION
  | ORGANIZATIONS -- plural to prevent naming conflict
  deriving (Generic, ToSchema, ToJSON, FromJSON, Read, Show, Eq, Enum, Bounded)

deriving instance HasSqlEqualityCheck Postgres DocumentEntity

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DocumentEntity where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres DocumentEntity where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData DocumentEntity where
  parseUrlPiece = parseBoundedTextData
  parseQueryParam = parseBoundedTextData
  parseHeader = parseBoundedTextData . DT.decodeUtf8

data DocumentByType
  = VERIFIER
  | CREATOR
  deriving (Generic, ToSchema, ToJSON, FromJSON, Read, Show, Eq, Enum, Bounded)

deriving instance HasSqlEqualityCheck Postgres DocumentByType

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DocumentByType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres DocumentByType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData DocumentByType where
  parseUrlPiece = parseBoundedTextData
  parseQueryParam = parseBoundedTextData
  parseHeader = parseBoundedTextData . DT.decodeUtf8

data LocateBy
  = LSTATE
  | LCITY
  | LDISTRICT
  | LWARD
  | LPINCODE
  deriving (Generic, ToSchema, ToJSON, FromJSON, Read, Show, Eq, Enum, Bounded)

instance FromHttpApiData LocateBy where
  parseUrlPiece = parseBoundedTextData
  parseQueryParam = parseBoundedTextData
  parseHeader = parseBoundedTextData . DT.decodeUtf8
