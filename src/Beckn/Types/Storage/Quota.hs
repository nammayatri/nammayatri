{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}


module Beckn.Types.Storage.Quota where

import           Data.Aeson
import           Data.Time
import           EulerHS.Prelude

import qualified Database.Beam   as B

data QuotaT f =
  Quota
    { _id         :: B.C f Text
    , _maxAllowed :: B.C f Int
    , _type       :: B.C f Text
    , _EntityId   :: B.C f Text
    , _entityType :: B.C f Text
    , _startTime  :: B.C f LocalTime
    , _endTime    :: B.C f LocalTime
    , _createdAt  :: B.C f LocalTime
    , _updatedAt  :: B.C f LocalTime
    }
  deriving (Generic, B.Beamable)

type Quota = QuotaT Identity

type QuotaPrimaryKey = B.PrimaryKey QuotaT Identity

instance B.Table QuotaT where
  data PrimaryKey QuotaT f = QuotaPrimaryKey (B.C f Text)
                               deriving (Generic, B.Beamable)
  primaryKey = QuotaPrimaryKey . _id

deriving instance Show Quota

deriving instance Eq Quota

deriving instance ToJSON Quota

deriving instance FromJSON Quota

fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity QuotaT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      {
      _maxAllowed = "max_allowed"
      , _EntityId = "entity_id"
      , _entityType = "entity_type"
      , _createdAt = "created_at"
      , _updatedAt = "updated_at"
      }


