{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Products where

import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Servant.API
import qualified Types.Storage.SearchRequest as SearchRequest

data ProductsType = RIDE | PASS
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductsType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres ProductsType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

type ProductsIndustry = SearchRequest.Industry

-- data ProductsIndustry = MOBILITY | GOVT | GROCERY
--   deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductsIndustry where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance FromBackendRow Postgres ProductsIndustry where
--   fromBackendRow = read . T.unpack <$> fromBackendRow

data ProductsStatus = INSTOCK | OUTOFSTOCK
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductsStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres ProductsStatus

instance FromBackendRow Postgres ProductsStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData ProductsStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data ProductsT f = Products
  { id :: B.C f (Id Products),
    name :: B.C f Text,
    description :: B.C f (Maybe Text),
    industry :: B.C f ProductsIndustry,
    _type :: B.C f ProductsType,
    shortId :: B.C f (ShortId Products),
    status :: B.C f ProductsStatus,
    price :: B.C f Amount,
    rating :: B.C f (Maybe Text),
    review :: B.C f (Maybe Text),
    info :: B.C f (Maybe Text),
    udf1 :: B.C f (Maybe Text),
    udf2 :: B.C f (Maybe Text),
    udf3 :: B.C f (Maybe Text),
    udf4 :: B.C f (Maybe Text),
    udf5 :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Products = ProductsT Identity

type ProductsPrimaryKey = B.PrimaryKey ProductsT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table ProductsT where
  data PrimaryKey ProductsT f = ProductsPrimaryKey (B.C f (Id Products))
    deriving (Generic, B.Beamable)
  primaryKey = ProductsPrimaryKey . id

deriving instance Show Products

deriving instance Eq Products

instance ToJSON Products where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Products where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity ProductsT)
fieldEMod =
  B.setEntityName "product"
    <> B.modifyTableFields
      B.tableModification
        { shortId = "short_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }

validateStatusTransition :: ProductsStatus -> ProductsStatus -> Either Text ()
validateStatusTransition oldState newState =
  if oldState == newState
    then allowed
    else t oldState newState
  where
    forbidden =
      Left $
        T.pack $
          "It is not allowed to change Product status from "
            <> show oldState
            <> " to "
            <> show newState
    allowed = Right ()
    t INSTOCK OUTOFSTOCK = allowed
    t INSTOCK _ = forbidden
    t OUTOFSTOCK INSTOCK = allowed
    t OUTOFSTOCK _ = forbidden
