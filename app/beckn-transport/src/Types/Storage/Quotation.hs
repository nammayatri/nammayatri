{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Types.Storage.Quotation where

import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Servant.API

data Status = NEW | PENDING | EXPIRED | CONFIRMED | SYSTEM_CANCELLED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Status where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Status

instance FromHttpApiData Status where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data QuotationT f = Quotation
  { id :: B.C f (Id Quotation),
    leadsId :: B.C f Text,
    amount :: B.C f Text,
    organizationId :: B.C f Text,
    status :: B.C f Status,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Quotation = QuotationT Identity

type QuotationPrimaryKey = B.PrimaryKey QuotationT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table QuotationT where
  data PrimaryKey QuotationT f = QuotationPrimaryKey (B.C f (Id Quotation))
    deriving (Generic, B.Beamable)
  primaryKey = QuotationPrimaryKey . id

deriving instance Show Quotation

deriving instance Eq Quotation

instance ToJSON Quotation where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Quotation where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema Quotation

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity QuotationT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { createdAt = "created_at",
        updatedAt = "updated_at",
        leadsId = "booking_reference_id",
        organizationId = "organization_id"
      }
