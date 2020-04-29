{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}


module Beckn.Types.Storage.LocationBlacklist where

import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Utils.Defaults as Defaults
import           Data.Aeson
import           Data.Default
import           Data.Time
import qualified Database.Beam        as B
import           EulerHS.Prelude

data LocationBlacklistT f =
  LocationBlacklist
    { _id                   :: B.C f LocationBlacklistId
    , _BlacklistedBy        :: B.C f UserId
    , _TenantOrganizationId :: B.C f (Maybe TenantOrganizationId)
    , _type                 :: B.C f LocationType
    , _remarks              :: B.C f Text
    , _bound                :: B.C f (Maybe Bound)
    , _district             :: B.C f (Maybe Text)
    , _city                 :: B.C f (Maybe Text)
    , _state                :: B.C f (Maybe Text)
    , _ward                 :: B.C f (Maybe Text)
    , _country              :: B.C f Text
    , _pincode              :: B.C f (Maybe Int)
    , _info                 :: B.C f (Maybe Text)
    , _createdAt            :: B.C f LocalTime
    , _updatedAt            :: B.C f LocalTime
    }
  deriving (Generic, B.Beamable)

type LocationBlacklist = LocationBlacklistT Identity

type LocationBlacklistPrimaryKey = B.PrimaryKey LocationBlacklistT Identity


instance Default LocationBlacklist where
  def = LocationBlacklist
    { _id            = LocationBlacklistId Defaults.id
    , _remarks       = ""
    , _BlacklistedBy = UserId Defaults.id
    , _TenantOrganizationId = Nothing
    , _type          = Defaults.locationType
    , _info          = Nothing
    , _bound         = Nothing
    , _district      = Just Defaults.district
    , _city          = Just Defaults.city
    , _state         = Just Defaults.state
    , _ward          = Nothing
    , _country       = Defaults.country
    , _pincode       = Just Defaults.pincode
    , _createdAt     = Defaults.localTime
    , _updatedAt     = Defaults.localTime
    }


instance B.Table LocationBlacklistT where
  data PrimaryKey LocationBlacklistT f = LocationBlacklistPrimaryKey (B.C f LocationBlacklistId)
                               deriving (Generic, B.Beamable)
  primaryKey = LocationBlacklistPrimaryKey . _id

deriving instance Show LocationBlacklist

deriving instance Eq LocationBlacklist

deriving instance FromJSON LocationBlacklist

instance ToJSON LocationBlacklist where
  toJSON = genericToJSON stripLensPrefixOptions

insertExpression location_blacklist = insertExpressions [location_blacklist]

insertExpressions location_blacklists = B.insertValues location_blacklists

fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity LocationBlacklistT)
fieldEMod =
  B.setEntityName "location_blacklist" <>
    B.modifyTableFields
      B.tableModification
        { _BlacklistedBy = "blacklisted_by"
        , _TenantOrganizationId = "tenant_organization_id"
        , _createdAt = "created_at"
        , _updatedAt = "updated_at"
        }


