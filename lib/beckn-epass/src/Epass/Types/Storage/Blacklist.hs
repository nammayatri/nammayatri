{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}


module Epass.Types.Storage.Blacklist where

import           Epass.Types.App
import           Epass.Types.Common
import qualified Epass.Utils.Defaults as Defaults
import           Data.Aeson
import           Data.Default
import           Data.Swagger
import           Data.Time
import qualified Database.Beam        as B
import           EulerHS.Prelude

data BlacklistT f =
  Blacklist
    { _id                   :: B.C f BlacklistId
    , __BlacklistedBy        :: B.C f UserId
    , __TenantOrganizationId :: B.C f (Maybe TenantOrganizationId)
    , _remarks              :: B.C f Text
    , __EntityId             :: B.C f Text
    , _entityType           :: B.C f EntityType
    , _startTime            :: B.C f LocalTime
    , _endTime              :: B.C f LocalTime
    , _info                 :: B.C f (Maybe Text)
    , _createdAt            :: B.C f LocalTime
    , _updatedAt            :: B.C f LocalTime
    }
  deriving (Generic, B.Beamable)

type Blacklist = BlacklistT Identity

type BlacklistPrimaryKey = B.PrimaryKey BlacklistT Identity


instance Default Blacklist where
  def = Blacklist
    { _id                    = BlacklistId Defaults.id
    , _remarks               = ""
    , __BlacklistedBy         = UserId Defaults.id
    , __TenantOrganizationId  = Nothing
    , _info                  = Nothing
    , _startTime             = Defaults.localTime
    , _endTime               = Defaults.localTime
    , __EntityId              = Defaults.orgId
    , _entityType            = LOCATION
    , _createdAt             = Defaults.localTime
    , _updatedAt             = Defaults.localTime
    }


instance B.Table BlacklistT where
  data PrimaryKey BlacklistT f = BlacklistPrimaryKey (B.C f BlacklistId)
                               deriving (Generic, B.Beamable)
  primaryKey = BlacklistPrimaryKey . _id

deriving instance Show Blacklist

deriving instance Eq Blacklist

instance ToSchema Blacklist

instance FromJSON Blacklist where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Blacklist where
  toJSON = genericToJSON stripAllLensPrefixOptions

insertExpression blacklist = insertExpressions [blacklist]

insertExpressions blacklists = B.insertValues blacklists

fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity BlacklistT)
fieldEMod =
  B.setEntityName "blacklist" <>
    B.modifyTableFields
      B.tableModification
        { __BlacklistedBy = "blacklisted_by"
        , __TenantOrganizationId = "tenant_organization_id"
        , __EntityId = "entity_id"
        , _entityType = "entity_type"
        , _startTime = "start_time"
        , _endTime = "end_time"
        , _createdAt = "created_at"
        , _updatedAt = "updated_at"
        }


