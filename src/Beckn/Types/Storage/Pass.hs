{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Pass where

import           Beckn.Types.App
import qualified Data.Text                 as T
import           Data.Time.LocalTime
import qualified Database.Beam             as B
import           Database.Beam.Backend.SQL
import           Database.Beam.MySQL
import           EulerHS.Prelude

data Status = ACTIVE | REVOKED | EXPIRED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL Status where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data PassType = INDIVIDUAL | ORGANIZATION
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be PassType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL PassType where
  fromBackendRow = read . T.unpack <$> fromBackendRow


data PassT f =
  Pass
    { _id                :: B.C f PassId
    , _CustomerId        :: B.C f CustomerId
    , _OrganizationId    :: B.C f OrganizationId
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

deriving instance ToJSON Pass

deriving instance FromJSON Pass

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
