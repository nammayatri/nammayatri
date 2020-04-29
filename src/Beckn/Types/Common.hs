{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

module Beckn.Types.Common where

import           Data.Aeson
import qualified Data.Aeson                as Aeson
import qualified Data.ByteString.Lazy      as BSL
import           Data.Default
import           Data.Swagger
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as DT
import           Database.Beam.Backend.SQL
import           Database.Beam.MySQL
import           Database.Beam.Query       (HasSqlEqualityCheck)
import           EulerHS.Prelude
import           Servant
import           Servant.Swagger

data ErrorResponse =
  ErrorResponse
    { status          :: Text
    , responseCode    :: Text
    , responseMessage :: Text
    }
  deriving (Show, Generic, ToJSON, ToSchema)

data LoginMode
  = VERIFY
  | RESEND
  deriving (Generic, FromJSON, ToSchema)

data PassApplicationType
  = SELF
  | SPONSOROR
  | BULKSPONSOROR
  deriving (Eq, Generic, FromJSON, ToSchema)

instance Default PassApplicationType where
  def = SELF

data TravellerIDType
  = MOBILE
  | AADHAR
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

instance FromBackendRow MySQL PassType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData PassType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

data LocationType
  = POINT
  | POLYGON
  | PINCODE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be LocationType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL LocationType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData LocationType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

data Location =
  Location
    { _type     :: LocationType
    , _lat      :: Maybe Double
    , _long     :: Maybe Double
    , _ward     :: Maybe Text
    , _district :: Maybe Text
    , _city     :: Maybe Text
    , _state    :: Maybe Text
    , _country  :: Maybe Text
    , _pincode  :: Maybe Text
    , _address  :: Maybe Text
    , _bound    :: Maybe Bound
    }
  deriving (Show, Generic, ToSchema)

instance FromJSON Location where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Location where
  toJSON = genericToJSON stripAllLensPrefixOptions

newtype Bound =
  Bound Value
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance ToSchema Bound where
  declareNamedSchema _ =
    return $ NamedSchema (Just "Bound") (sketchSchema Aeson.Null)

deriving newtype instance HasSqlValueSyntax MysqlValueSyntax Bound

deriving newtype instance FromBackendRow MySQL Bound

data QuotaType
  = DAILY
  | HOURLY
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be QuotaType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL QuotaType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData QuotaType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

data EntityType
  = LOCATION
  | ORG
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)


deriving instance HasSqlEqualityCheck MySQL EntityType

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EntityType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL EntityType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData EntityType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

data Ack =
  Ack
    { _action  :: Text
    , _message :: Text
    }
  deriving (Generic, Show)

instance FromJSON Ack where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Ack where
  toJSON = genericToJSON stripLensPrefixOptions
