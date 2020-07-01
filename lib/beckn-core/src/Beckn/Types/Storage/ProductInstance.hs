{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.ProductInstance where

import Beckn.Types.App
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Generics.Labels
import Data.Scientific
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time.LocalTime
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude
import Servant.API
import Servant.Swagger

-- TODO: INVALID status seems to be unused
data ProductInstanceStatus = VALID | INVALID | INPROGRESS | CONFIRMED | COMPLETED | INSTOCK | OUTOFSTOCK | CANCELLED | EXPIRED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductInstanceStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres ProductInstanceStatus

instance FromBackendRow Postgres ProductInstanceStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData ProductInstanceStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

data ProductInstanceT f = ProductInstance
  { _id :: B.C f ProductInstanceId,
    _caseId :: B.C f CaseId,
    _productId :: B.C f ProductsId,
    _personId :: B.C f (Maybe PersonId),
    _quantity :: B.C f Int,
    _price :: B.C f Scientific,
    _status :: B.C f ProductInstanceStatus,
    _info :: B.C f (Maybe Text),
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

type ProductInstance = ProductInstanceT Identity

type ProductInstancePrimaryKey = B.PrimaryKey ProductInstanceT Identity

instance B.Table ProductInstanceT where
  data PrimaryKey ProductInstanceT f = ProductInstancePrimaryKey (B.C f ProductInstanceId)
    deriving (Generic, B.Beamable)
  primaryKey = ProductInstancePrimaryKey . _id

deriving instance Show ProductInstance

deriving instance Eq ProductInstance

instance ToJSON ProductInstance where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON ProductInstance where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema ProductInstance

insertExpression products = insertExpressions [products]

insertExpressions products = B.insertValues products

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity ProductInstanceT)
fieldEMod =
  B.setEntityName "product_instance"
    <> B.modifyTableFields
      B.tableModification
        { _caseId = "case_id",
          _productId = "product_id",
          _personId = "person_id",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }

validateStatusTransition :: ProductInstanceStatus -> ProductInstanceStatus -> Either Text ()
validateStatusTransition oldState newState =
  if oldState == newState
    then allowed
    else t oldState newState
  where
    forbidden =
      Left $ T.pack $
        "It is not allowed to change Product Instance status from "
          <> show oldState
          <> " to "
          <> show newState
    allowed = Right ()
    t VALID CONFIRMED = allowed
    t VALID EXPIRED = allowed
    t VALID _ = forbidden
    t CONFIRMED INPROGRESS = allowed
    t CONFIRMED CANCELLED = allowed
    t CONFIRMED _ = forbidden
    t INPROGRESS COMPLETED = allowed
    t INPROGRESS CANCELLED = allowed
    t INPROGRESS _ = forbidden
    t COMPLETED _ = forbidden
    t INSTOCK CONFIRMED = allowed
    t CANCELLED _ = forbidden
    t EXPIRED _ = forbidden
    t INSTOCK _ = forbidden
    t OUTOFSTOCK _ = forbidden
    t INVALID _ = forbidden
