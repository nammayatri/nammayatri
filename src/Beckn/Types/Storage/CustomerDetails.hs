{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.CustomerDetails where

import           Beckn.Types.App
import           Data.Aeson
import qualified Data.Text                 as T
import           Data.Time.LocalTime
import qualified Database.Beam             as B
import           Database.Beam.Backend.SQL
import           Database.Beam.MySQL
import           EulerHS.Prelude

data IdentifierType = MOBILENUMBER | AADHAR
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be IdentifierType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be IdentifierType
instance FromBackendRow MySQL IdentifierType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data CustomerDetailsT f =
  CustomerDetails
    { _id                :: B.C f CustomerDetailsId
    , _CustomerId        :: B.C f CustomerId
    , _uniqueIdentifier  :: B.C f Text
    , _identifierType    :: B.C f IdentifierType
    , _value             :: B.C f Value
    , _verified          :: B.C f Bool
    , _primaryIdentifier :: B.C f Bool
    , _info              :: B.C f Text
    , _createdAt         :: B.C f LocalTime
    , _updatedAt         :: B.C f LocalTime
    }
  deriving (Generic, B.Beamable)

type CustomerDetails = CustomerDetailsT Identity

type CustomerDetailsPrimaryKey = B.PrimaryKey CustomerDetailsT Identity

instance B.Table CustomerDetailsT where
  data PrimaryKey CustomerDetailsT f = CustomerDetailsPrimaryKey (B.C f CustomerDetailsId)
                               deriving (Generic, B.Beamable)
  primaryKey = CustomerDetailsPrimaryKey . _id

deriving instance Show CustomerDetails

deriving instance Eq CustomerDetails

deriving instance ToJSON CustomerDetails

deriving instance FromJSON CustomerDetails

insertExpression customer = insertExpressions [customer]

insertExpressions customers = B.insertValues customers

fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CustomerDetailsT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _CustomerId = "customer_id"
      , _uniqueIdentifier = "unique_identifier"
      , _identifierType = "identifier_type"
      , _primaryIdentifier = "primary_identifier"
      , _createdAt = "created_at"
      , _updatedAt = "updated_at"
      }
