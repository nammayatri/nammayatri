{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}


module Beckn.Types.Storage.Quota where

import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Utils.Defaults as Defaults
import           Data.Aeson
import           Data.Default
import           Data.Swagger
import           Data.Time
import           EulerHS.Prelude


import qualified Database.Beam        as B

data QuotaT f =
  Quota
    { _id                   :: B.C f QuotaId
    , _maxAllowed           :: B.C f Int
    , _TenantOrganizationId :: B.C f TenantOrganizationId
    , _quotaType            :: B.C f QuotaType
    , _EntityId             :: B.C f Text
    , _entityType           :: B.C f EntityType
    , _startTime            :: B.C f LocalTime
    , _endTime              :: B.C f LocalTime
    , _createdAt            :: B.C f LocalTime
    , _updatedAt            :: B.C f LocalTime
    , _info                 :: B.C f (Maybe Text)
    }
  deriving (Generic, B.Beamable)

type Quota = QuotaT Identity

type QuotaPrimaryKey = B.PrimaryKey QuotaT Identity

instance B.Table QuotaT where
  data PrimaryKey QuotaT f = QuotaPrimaryKey (B.C f QuotaId)
                               deriving (Generic, B.Beamable)
  primaryKey = QuotaPrimaryKey . _id


instance Default Quota where
  def = Quota
    { _id         = QuotaId Defaults.id
    , _maxAllowed = 1000
    , _quotaType = HOURLY
    , _EntityId   = Defaults.orgId
    , _entityType = ORG
    , _TenantOrganizationId = TenantOrganizationId Defaults.orgId
    , _startTime  = Defaults.localTime
    , _endTime    = Defaults.localTime
    , _createdAt  = Defaults.localTime
    , _updatedAt  = Defaults.localTime
    , _info       = Nothing
    }

instance ToJSON Quota where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Quota where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

deriving instance Show Quota

deriving instance Eq Quota

instance ToSchema Quota

insertExpression quota = insertExpressions [quota]

insertExpressions quotas = B.insertValues quotas

fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity QuotaT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      {
      _maxAllowed = "max_allowed"
      , _EntityId = "entity_id"
      , _quotaType = "quota_type"
      , _TenantOrganizationId = "tenant_organization_id"
      , _entityType = "entity_type"
      , _startTime = "start_time"
      , _endTime = "end_time"
      , _createdAt = "created_at"
      , _updatedAt = "updated_at"
      }


