{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE UndecidableInstances #-}


module Beckn.Types.Storage.RegistrationToken where

import           Data.Aeson
import           Data.Time
import           EulerHS.Prelude
import           Database.Beam.Backend.SQL
import           Database.Beam.MySQL
import qualified Data.Text       as T
import qualified Database.Beam   as B
import           Data.Swagger

data Medium
  = SMS
  | EMAIL
  deriving (Generic, FromJSON, ToJSON, ToSchema, Eq, Show, Read)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Medium where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL Medium where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data LoginType
  = OTP
  | PASSWORD
  deriving (Generic, FromJSON, ToJSON, ToSchema, Eq, Show, Read)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be LoginType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL LoginType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data RegistrationTokenT f =
  RegistrationToken
    { _id            :: B.C f Text
    , _token         :: B.C f Text
    , _attempts      :: B.C f Int
    , _authMedium    :: B.C f Medium
    , _authType      :: B.C f LoginType
    , _authValueHash :: B.C f Text
    , _verified      :: B.C f Bool
    , _authExpiry    :: B.C f Int
    , _tokenExpiry   :: B.C f Int
    , _CustomerId    :: B.C f Text
    , _createdAt     :: B.C f LocalTime
    , _updatedAt     :: B.C f LocalTime
    , _info          :: B.C f (Maybe Text)
    }
  deriving (Generic, B.Beamable)

type RegistrationToken = RegistrationTokenT Identity

type RegistrationTokenPrimaryKey = B.PrimaryKey RegistrationTokenT Identity

instance B.Table RegistrationTokenT where
  data PrimaryKey RegistrationTokenT f = RegistrationTokenPrimaryKey (B.C f Text)
                               deriving (Generic, B.Beamable)
  primaryKey = RegistrationTokenPrimaryKey . _id

deriving instance Show RegistrationToken

deriving instance Eq RegistrationToken

deriving instance ToJSON RegistrationToken

deriving instance FromJSON RegistrationToken

insertExpression regs = insertExpressions [regs]

insertExpressions regs = B.insertValues regs

fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RegistrationTokenT)
fieldEMod =
  B.setEntityName "registration_token" <>
    B.modifyTableFields
      B.tableModification
        {_authMedium  = "auth_medium"
        , _authType  =  "auth_type"
        , _authValueHash = "auth_value_hash"
        , _authExpiry  = "auth_expiry"
        , _CustomerId  = "customer_id"
        , _createdAt = "created_at"
        , _updatedAt = "updated_at"
        }


