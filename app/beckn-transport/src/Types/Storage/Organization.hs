{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Organization where

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

data Status = PENDING_VERIFICATION | APPROVED | REJECTED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

data OrganizationType = GATEWAY | PROVIDER | TRANSPORTER
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL Status where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Status
instance FromHttpApiData Status where
  parseUrlPiece  = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

instance HasSqlValueSyntax be String => HasSqlValueSyntax be OrganizationType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL OrganizationType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema OrganizationType
instance FromHttpApiData OrganizationType where
  parseUrlPiece  = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

data OrganizationT f =
  Organization
    { _id           :: B.C f OrganizationId
    , _name         :: B.C f Text
    , _gstin        :: B.C f (Maybe Text)
    , _fcmId        :: B.C f (Maybe Text)
    , _status       :: B.C f Status
    , _type         :: B.C f OrganizationType
    , _verified     :: B.C f Bool
    , _locationId   :: B.C f (Maybe Text)
    , _info         :: B.C f (Maybe Text)
    , _createdAt    :: B.C f LocalTime
    , _updatedAt    :: B.C f LocalTime
    }

  deriving (Generic, B.Beamable)

type Organization = OrganizationT Identity

type OrganizationPrimaryKey = B.PrimaryKey OrganizationT Identity

instance B.Table OrganizationT where
  data PrimaryKey OrganizationT f = OrganizationPrimaryKey (B.C f OrganizationId)
                               deriving (Generic, B.Beamable)
  primaryKey = OrganizationPrimaryKey . _id

deriving instance Show Organization

deriving instance Eq Organization

instance ToJSON Organization where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Organization where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Organization

insertExpression org = insertExpressions [org]

insertExpressions orgs = B.insertValues orgs


fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity OrganizationT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _createdAt = "created_at"
      , _updatedAt = "updated_at"
      , _locationId = "location_id"
      }
