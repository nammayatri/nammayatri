{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Types.Storage.Customer where

import Beckn.External.Encryption
import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import Data.Swagger
import Data.Time
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)

data CustomerTE e f = Customer
  { id :: B.C f (Id Customer),
    referenceId :: B.C f Text,
    name :: B.C f Text,
    mobileNumber :: EncryptedHashedField e f Text,
    info :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic)

type CustomerT = CustomerTE 'AsEncrypted

type Customer = CustomerTE 'AsUnencrypted Identity

instance B.Beamable CustomerT

type CustomerPrimaryKey = B.PrimaryKey CustomerT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table CustomerT where
  data PrimaryKey CustomerT f = CustomerPrimaryKey (B.C f (Id Customer))
    deriving (Generic, B.Beamable)
  primaryKey = CustomerPrimaryKey . id

deriving instance Show Customer

deriving instance Eq Customer

instance ToJSON Customer where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Customer where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema Customer

instance ToJSON (CustomerT Identity)

instance FromJSON (CustomerT Identity)

deriveTableEncryption ''CustomerTE

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CustomerT)
fieldEMod =
  B.modifyTableFields
    (B.tableModification @_ @CustomerT)
      { createdAt = "created_at",
        updatedAt = "updated_at",
        mobileNumber =
          EncryptedHashed
            { encrypted = "mobile_number_encrypted",
              hash = "mobile_number_hash"
            },
        referenceId = "reference_id"
      }
