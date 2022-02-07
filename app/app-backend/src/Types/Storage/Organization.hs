{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Organization where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Servant.API

data OrganizationType
  = PROVIDER
  | GATEWAY
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be OrganizationType where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres OrganizationType

instance FromBackendRow Postgres OrganizationType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData OrganizationType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

data OrganizationT f = Organization
  { id :: B.C f (Id Organization),
    shortId :: B.C f (ShortId Organization),
    _type :: B.C f OrganizationType
  }
  deriving (Generic, B.Beamable)

type Organization = OrganizationT Identity

type OrganizationPrimaryKey = B.PrimaryKey OrganizationT Identity

instance B.Table OrganizationT where
  data PrimaryKey OrganizationT f = OrganizationPrimaryKey (B.C f (Id Organization))
    deriving (Generic, B.Beamable)
  primaryKey = OrganizationPrimaryKey . (.id)

deriving instance Show Organization

deriving instance Eq Organization

instance ToJSON Organization where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Organization where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity OrganizationT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { shortId = "short_id"
      }
