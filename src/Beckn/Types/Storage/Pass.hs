{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Pass where

import           Beckn.Types.App
import           Beckn.Types.Common        (PassType (..))
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

data Status = ACTIVE | REVOKED | EXPIRED
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

data PassT f =
  Pass
    { _id                :: B.C f PassId
    , _CustomerId        :: B.C f CustomerId
    , _OrganizationId    :: B.C f (Maybe OrganizationId)
    , _status            :: B.C f Status
    , _fromDate          :: B.C f LocalTime
    , _toDate            :: B.C f LocalTime
    , _type              :: B.C f PassType
    , _PassApplicationId :: B.C f PassApplicationId
    , _FromLocationId    :: B.C f LocationId
    , _ToLocationId      :: B.C f LocationId
    , _CreatedBy         :: B.C f CustomerId
    , _info              :: B.C f Text
    , _createdAt         :: B.C f LocalTime
    , _updatedAt         :: B.C f LocalTime
    }
  deriving (Generic, B.Beamable)

type Pass = PassT Identity

type PassPrimaryKey = B.PrimaryKey PassT Identity

instance B.Table PassT where
  data PrimaryKey PassT f = PassPrimaryKey (B.C f PassId)
                               deriving (Generic, B.Beamable)
  primaryKey = PassPrimaryKey . _id

deriving instance Show Pass

deriving instance Eq Pass

instance ToJSON Pass where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Pass where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Pass

insertExpression customer = insertExpressions [customer]

insertExpressions customers = B.insertValues customers


fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity PassT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _OrganizationId = "organization_id"
      , _CustomerId = "customer_id"
      , _fromDate = "from_date"
      , _toDate = "to_date"
      , _PassApplicationId = "pass_application_id"
      , _FromLocationId = "from_locationId"
      , _ToLocationId = "to_locationId"
      , _CreatedBy = "created_by"
      , _createdAt = "created_at"
      , _updatedAt = "updated_at"
      }
