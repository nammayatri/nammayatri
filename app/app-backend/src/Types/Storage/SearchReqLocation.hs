{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.SearchReqLocation where

import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id, state)
import qualified Types.Common as Common
import Types.Error
import Utils.Common hiding (id)
import Data.OpenApi (ToSchema)

data SearchReqLocationT f = SearchReqLocation
  { id :: B.C f (Id SearchReqLocation),
    lat :: B.C f Double,
    long :: B.C f Double,
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
        { createdAt = "created_at",
          updatedAt = "updated_at"
        }

data SearchReqLocationAPIEntity = SearchReqLocationAPIEntity
  { address :: Common.Address,
    gps :: Common.GPS
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeSearchReqLocationAPIEntity :: SearchReqLocation -> SearchReqLocationAPIEntity
makeSearchReqLocationAPIEntity loc = do
  let address =
        Common.Address
          { door = "",
            building = "",
            street = "",
            area = "",
            city = fromMaybe "" loc.city,
            country = fromMaybe "" loc.country,
            areaCode = "",
            state = fromMaybe "" loc.state
          }
      gps =
        Common.GPS
          { lat = show loc.lat,
            lon = show loc.long
          }
  SearchReqLocationAPIEntity
    { ..
    }

buildSearchReqLoc :: MonadFlow m => SearchReqLocationAPIEntity -> m SearchReqLocation
buildSearchReqLoc SearchReqLocationAPIEntity {..} = do
  now <- getCurrentTime
  locId <- generateGUID
  lat <- readMaybe (T.unpack gps.lat) & fromMaybeM (InvalidRequest "Lat field is not present.")
  lon <- readMaybe (T.unpack gps.lon) & fromMaybeM (InvalidRequest "Lon field is not present.")
  return
    SearchReqLocation
      { id = locId,
        lat = lat,
        long = lon,
        district = Nothing,
        city = Just address.city,
        state = Just address.state,
        country = Just address.country,
        pincode = Nothing,
        address = Just . T.decodeUtf8 . BSL.toStrict $ encode address,
        createdAt = now,
        updatedAt = now
      }