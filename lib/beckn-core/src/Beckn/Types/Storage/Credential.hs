{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Credential where

import Beckn.Types.Storage.Organization (OrganizationDomain)
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude

data CredentialT f = Credential
  { _uniqueKeyId :: B.C f Text,
    _shortOrgId :: B.C f Text,
    _domain :: B.C f (Maybe OrganizationDomain),
    _city :: B.C f (Maybe Text),
    _country :: B.C f (Maybe Text),
    _signPubKey :: B.C f Text,
    _signPrivKey :: B.C f (Maybe Text),
    _encPubKey :: B.C f Text,
    _encPrivKey :: B.C f (Maybe Text),
    _validFrom :: B.C f UTCTime,
    _validTill :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Credential = CredentialT Identity

type CredentialPrimaryKey = B.PrimaryKey CredentialT Identity

instance B.Table CredentialT where
  data PrimaryKey CredentialT f = CredentialPrimaryKey (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = CredentialPrimaryKey . _uniqueKeyId

deriving instance Show Credential

deriving instance Eq Credential

-- FIXME: Are these needed? Beam defaults seem to cover everything we have
fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CredentialT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _uniqueKeyId = "unique_key_id",
        _shortOrgId = "short_org_id",
        _signPubKey = "sign_pub_key",
        _signPrivKey = "sign_priv_key",
        _encPubKey = "enc_pub_key",
        _encPrivKey = "enc_priv_key",
        _validFrom = "valid_from",
        _validTill = "valid_till"
      }
