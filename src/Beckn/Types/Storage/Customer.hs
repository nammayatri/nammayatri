{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Customer where

import           Beckn.Types.App
import qualified Data.Text                 as T
import           Data.Time.LocalTime
import qualified Database.Beam             as B
import           Database.Beam.Backend.SQL
import           Database.Beam.MySQL
import           EulerHS.Prelude
import           Servant.Swagger
import           Data.Swagger

data CustomerRole = BUSINESSADMIN | INDIVIDUAL
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CustomerRole where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL CustomerRole where
  fromBackendRow = read . T.unpack <$> fromBackendRow


data CustomerT f =
  Customer
    { _id             :: B.C f CustomerId
    , _name           :: B.C f (Maybe Text)
    , _OrganizationId :: B.C f (Maybe OrganizationId)
    , _verified       :: B.C f Bool
    , _role           :: B.C f CustomerRole
    , _info           :: B.C f (Maybe Text)
    , _createdAt      :: B.C f LocalTime
    , _updatedAt      :: B.C f LocalTime
    }
  deriving (Generic, B.Beamable)

type Customer = CustomerT Identity

type CustomerPrimaryKey = B.PrimaryKey CustomerT Identity

instance B.Table CustomerT where
  data PrimaryKey CustomerT f = CustomerPrimaryKey (B.C f CustomerId)
                               deriving (Generic, B.Beamable)
  primaryKey = CustomerPrimaryKey . _id

deriving instance Show Customer

deriving instance Eq Customer

deriving instance ToJSON Customer

deriving instance FromJSON Customer

instance ToSchema Customer

insertExpression customer = insertExpressions [customer]

insertExpressions customers = B.insertValues customers


fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CustomerT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _OrganizationId = "organization_id"
      , _createdAt = "created_at"
      , _updatedAt = "updated_at"
      }
