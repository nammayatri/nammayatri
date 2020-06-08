{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Customer where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
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
import Types.App

data CustomerT f
  = Customer
      { _id :: B.C f CustomerId,
        _referenceId :: B.C f Text,
        _name :: B.C f Text,
        _mobileNumber :: B.C f Text,
        _info :: B.C f (Maybe Text),
        _createdAt :: B.C f LocalTime,
        _updatedAt :: B.C f LocalTime
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

instance ToJSON Customer where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Customer where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Customer

insertExpression org = insertExpressions [org]

insertExpressions orgs = B.insertValues orgs

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CustomerT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _createdAt = "created_at",
        _updatedAt = "updated_at",
        _mobileNumber = "mobile_number",
        _referenceId = "reference_id"
      }
