{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}


module Beckn.Types.Storage.LocationBlacklist where

import           Data.Aeson
import           Data.Time
import           EulerHS.Prelude

import qualified Database.Beam   as B

data LocationBlacklistT f =
  LocationBlacklist
    { _id            :: B.C f Text
    , _locationId    :: B.C f Text
    , _BlacklistedBy :: B.C f Text
    , _remarks       :: B.C f Text
    , _info          :: B.C f Text
    , _createdAt     :: B.C f LocalTime
    , _updatedAt     :: B.C f LocalTime
    }
  deriving (Generic, B.Beamable)

type LocationBlacklist = LocationBlacklistT Identity

type LocationBlacklistPrimaryKey = B.PrimaryKey LocationBlacklistT Identity

instance B.Table LocationBlacklistT where
  data PrimaryKey LocationBlacklistT f = LocationBlacklistPrimaryKey (B.C f Text)
                               deriving (Generic, B.Beamable)
  primaryKey = LocationBlacklistPrimaryKey . _id

deriving instance Show LocationBlacklist

deriving instance Eq LocationBlacklist

deriving instance ToJSON LocationBlacklist

deriving instance FromJSON LocationBlacklist

fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity LocationBlacklistT)
fieldEMod =
  B.setEntityName "location_blacklist" <>
    B.modifyTableFields
      B.tableModification
        { _locationId = "location_id"
        , _BlacklistedBy = "blacklisted_by"
        , _createdAt = "created_at"
        , _updatedAt = "updated_at"
        }


