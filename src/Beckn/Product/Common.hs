{-# LANGUAGE TypeFamilies #-}

module Beckn.Product.Common where

import qualified Beckn.Data.Accessor                   as Accessor
import qualified Beckn.Storage.Queries.Blacklist       as DB
import qualified Beckn.Storage.Queries.EntityTag       as ETag
import qualified Beckn.Storage.Queries.Location        as DB
import qualified Beckn.Storage.Queries.PassApplication as DB
import qualified Beckn.Storage.Queries.Tag             as Tag
import           Beckn.Types.API.Location.CRUD
import           Beckn.Types.API.PassApplication
import           Beckn.Types.API.Common                as C
import           Beckn.Types.App
import           Beckn.Types.Common
import           Beckn.Types.Storage.Blacklist         as BL
import qualified Beckn.Types.Storage.EntityTag         as ET
import           Beckn.Types.Storage.Location          as L
import           Beckn.Types.Storage.PassApplication
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import           Beckn.Types.Storage.Tag               as T
import           Beckn.Utils.Common
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import qualified Data.List                             as List
import           Data.Map.Strict
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import           Servant


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
          ((\t@Tag {..} ->
              let TagId x = _id
               in (x, t)) <$>
           tags)
      eTagMap =
        fromListWith (++) ((\et@ET.EntityTag {..} -> (_EntityId, [et])) <$> eTags)
      mkTag eTag@ET.EntityTag {..} =
        C.TagInfo {tag = fromJust $ lookup _TagId tagMap, entityTag = eTag}
      mkTagInfo eTags = mkTag <$> eTags
      mkLInfo loc@L.Location {..} =
        C.LocationInfo
          { _location = loc
          , _blacklistInfo = lookup _id bMap
          , _tagInfo = fromMaybe [] (mkTagInfo <$> (lookup _id eTagMap))
          }
   in mkLInfo loc
