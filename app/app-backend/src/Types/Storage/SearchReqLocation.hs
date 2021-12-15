{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.SearchReqLocation where

import Beckn.Types.Id
import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id, state)

data SearchReqLocationT f = SearchReqLocation
  { id :: B.C f (Id SearchReqLocation),
    lat :: B.C f Double,
    lon :: B.C f Double,
    street :: B.C f (Maybe Text),
    door :: B.C f (Maybe Text),
    city :: B.C f (Maybe Text),
    state :: B.C f (Maybe Text),
    country :: B.C f (Maybe Text),
    building :: B.C f (Maybe Text),
    areaCode :: B.C f (Maybe Text),
    area :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type SearchReqLocation = SearchReqLocationT Identity

type SearchReqLocationPrimaryKey = B.PrimaryKey SearchReqLocationT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table SearchReqLocationT where
  data PrimaryKey SearchReqLocationT f = SearchReqLocationPrimaryKey (B.C f (Id SearchReqLocation))
    deriving (Generic, B.Beamable)
  primaryKey = SearchReqLocationPrimaryKey . id

deriving instance Show SearchReqLocation

deriving instance Eq SearchReqLocation

deriving instance FromJSON SearchReqLocation

deriving instance ToJSON SearchReqLocation

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity SearchReqLocationT)
fieldEMod =
  B.setEntityName "search_request_location"
    <> B.modifyTableFields
      B.tableModification
        { areaCode = "area_code",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }

data SearchReqLocationAPIEntity = SearchReqLocationAPIEntity
  { lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    door :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeSearchReqLocationAPIEntity :: SearchReqLocation -> SearchReqLocationAPIEntity
makeSearchReqLocationAPIEntity SearchReqLocation {..} =
  SearchReqLocationAPIEntity
    { ..
    }
