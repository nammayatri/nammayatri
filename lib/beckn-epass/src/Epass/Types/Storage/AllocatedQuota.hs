{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}


module Epass.Types.Storage.AllocatedQuota where

import           Data.Aeson
import           Data.Time
import           EulerHS.Prelude

import qualified Database.Beam   as B

data AllocatedQuotaT f =
  AllocatedQuota
    { _id             :: B.C f Text
    , _QuotaId        :: B.C f Text
    , _allocatedCount :: B.C f Int
    , _version        :: B.C f Int
    , _startTime      :: B.C f LocalTime
    , _endTime        :: B.C f LocalTime
    , _createdAt      :: B.C f LocalTime
    , _updatedAt      :: B.C f LocalTime
    , _info           :: B.C f (Maybe Text)
    }
  deriving (Generic, B.Beamable)

type AllocatedQuota = AllocatedQuotaT Identity

type AllocatedQuotaPrimaryKey = B.PrimaryKey AllocatedQuotaT Identity

instance B.Table AllocatedQuotaT where
  data PrimaryKey AllocatedQuotaT f = AllocatedQuotaPrimaryKey (B.C f Text)
                               deriving (Generic, B.Beamable)
  primaryKey = AllocatedQuotaPrimaryKey . _id

deriving instance Show AllocatedQuota

deriving instance Eq AllocatedQuota

instance ToJSON AllocatedQuota where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON AllocatedQuota where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity AllocatedQuotaT)
fieldEMod =
  B.setEntityName "allocated_quota" <>
    B.modifyTableFields
      B.tableModification
        { _QuotaId = "quota_id"
        , _allocatedCount = "allocated_count"
        , _startTime = "start_time"
        , _endTime = "end_time"
        , _createdAt = "created_at"
        , _updatedAt = "updated_at"
        }


