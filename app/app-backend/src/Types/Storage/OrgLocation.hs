{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.OrgLocation where

import Beckn.Types.Id
import Data.Aeson
import Data.Swagger
import Data.Time
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id, state)
import Types.Storage.Organization (Organization)

data OrgLocationT f = OrgLocation
  { orgId :: B.C f (Id Organization),
    lat :: B.C f (Maybe Double),
    long :: B.C f (Maybe Double),
    district :: B.C f (Maybe Text),
    city :: B.C f (Maybe Text),
    state :: B.C f (Maybe Text),
    country :: B.C f (Maybe Text),
    pincode :: B.C f (Maybe Text),
    address :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type OrgLocation = OrgLocationT Identity

type OrgLocationPrimaryKey = B.PrimaryKey OrgLocationT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table OrgLocationT where
  data PrimaryKey OrgLocationT f = OrgLocationPrimaryKey (B.C f (Id Organization))
    deriving (Generic, B.Beamable)
  primaryKey = OrgLocationPrimaryKey . orgId

deriving instance Show OrgLocation

deriving instance Eq OrgLocation

instance ToJSON OrgLocation

instance FromJSON OrgLocation

instance ToSchema OrgLocation

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity OrgLocationT)
fieldEMod =
  B.setEntityName "organization_location"
    <> B.modifyTableFields
      B.tableModification
        { orgId = "org_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }