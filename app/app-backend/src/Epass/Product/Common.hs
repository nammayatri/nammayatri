{-# LANGUAGE TypeFamilies #-}

module Epass.Product.Common where

import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import Data.Aeson
import qualified Data.List as List
import Data.Map.Strict
import qualified Epass.Data.Accessor as Accessor
import qualified Epass.Storage.Queries.Blacklist as DB
import qualified Epass.Storage.Queries.EntityTag as ETag
import qualified Epass.Storage.Queries.Location as DB
import qualified Epass.Storage.Queries.PassApplication as DB
import qualified Epass.Storage.Queries.Tag as Tag
import Epass.Types.API.Common as C
import Epass.Types.API.Location.CRUD
import Epass.Types.API.PassApplication
import Epass.Types.App
import Epass.Types.Common
import Epass.Types.Storage.Blacklist as BL
import qualified Epass.Types.Storage.EntityTag as ET
import Epass.Types.Storage.Location as L
import Epass.Types.Storage.PassApplication
import Epass.Types.Storage.Tag as T
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant

getLocationInfo :: Text -> L.Flow C.LocationInfo
getLocationInfo lid = do
  -- Find Location By Id
  location <- DB.findLocationWithErr lid
  -- Add Blacklist Information
  blacklist <- DB.findAllByEntityId LOCATION [lid]
  -- Get All tags for the location
  locationTags <- ETag.findAllById (EntityTagId <$> [lid])
  tags <- Tag.findAllById (tagId <$> locationTags)
  return $ mkLocationInfo blacklist locationTags tags location
  where
    locationId L.Location {..} = _id
    tagId ET.EntityTag {..} = TagId _TagId

mkLocationInfo ::
  [Blacklist] -> [ET.EntityTag] -> [Tag] -> L.Location -> C.LocationInfo
mkLocationInfo bLists eTags tags loc =
  let bMap = fromList ((\bl@Blacklist {..} -> (__EntityId, bl)) <$> bLists)
      tagMap =
        fromList
          ( ( \t@Tag {..} ->
                let TagId x = _id
                 in (x, t)
            )
              <$> tags
          )
      eTagMap =
        fromListWith (++) ((\et@ET.EntityTag {..} -> (_EntityId, [et])) <$> eTags)
      mkTag eTag@ET.EntityTag {..} =
        C.TagInfo {tag = fromJust $ lookup _TagId tagMap, entityTag = eTag}
      mkTagInfo eTags = mkTag <$> eTags
      mkLInfo loc@L.Location {..} =
        C.LocationInfo
          { _location = loc,
            _blacklistInfo = lookup _id bMap,
            _tagInfo = maybe [] mkTagInfo (lookup _id eTagMap)
          }
   in mkLInfo loc

mkUInfo :: Person.Person -> Maybe C.LocationInfo -> C.UserInfo
mkUInfo user mlocInfo = UserInfo {_user = user, _locationInfo = mlocInfo}
