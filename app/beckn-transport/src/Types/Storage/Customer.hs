{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Customer where

import Beckn.External.Encryption
import Data.Aeson
import Data.Swagger
import Data.Time.LocalTime
import qualified Database.Beam as B
import EulerHS.Prelude
import Types.App

data CustomerTE e f = Customer
  { _id :: B.C f CustomerId,
    _referenceId :: B.C f Text,
    _name :: B.C f Text,
    _mobileNumber :: EncryptedHashedField e f Text,
    _info :: B.C f (Maybe Text),
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic)

type CustomerT = CustomerTE 'AsEncrypted

type Customer = CustomerTE 'AsUnencrypted Identity

instance B.Beamable CustomerT

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

instance ToJSON (CustomerT Identity)

instance FromJSON (CustomerT Identity)

deriveTableEncryption ''CustomerTE

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CustomerT)
fieldEMod =
  B.modifyTableFields
    (B.tableModification @_ @CustomerT)
      { _createdAt = "created_at",
        _updatedAt = "updated_at",
        _mobileNumber =
          EncryptedHashed
            { _encrypted = "mobile_number_encrypted",
              _hash = "mobile_number_hash"
            },
        _referenceId = "reference_id"
      }
