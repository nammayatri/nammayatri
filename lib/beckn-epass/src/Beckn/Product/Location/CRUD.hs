module Beckn.Product.Location.CRUD where

import qualified Beckn.Data.Accessor                   as Accessor
import           Beckn.Product.Common
import qualified Beckn.Storage.Queries.Blacklist       as DB
import qualified Beckn.Storage.Queries.EntityTag       as ETag
import qualified Beckn.Storage.Queries.Location        as DB
import qualified Beckn.Storage.Queries.PassApplication as DB
import qualified Beckn.Storage.Queries.Tag             as Tag
import           Beckn.Types.API.Location.CRUD
import           Beckn.Types.API.PassApplication
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Common                    as Location (Location (..),
                                                                    LocationType)
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

-- WIP
listLocation ::
     Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> LocateBy -- distinct
  -> LocateBy -- filterby type
  -> Text -- filterby value
  -> FlowHandler ListLocationRes
listLocation regToken offset limit distinctBy filterByT filterBy =
  withFlowHandler $ do
    verifyToken regToken

    -- :TODO move the nubBy function to beam (or check nub suits )
    locations <-
      List.nubBy (\l1 l2 -> getF l1 l2 distinctBy) <$>
      (DB.findByStOrDistrict offset limit filterByT filterBy)
    let locIds = (locationId <$> locations)

  -- Add Blacklist Information
    blacklist <- DB.findAllByEntityId LOCATION locIds

  -- Get All tags for the location
    locationTags <- ETag.findAllById (EntityTagId <$> locIds)

    tags <- Tag.findAllById (tagId <$> locationTags)
    return
      ListLocationRes
        { _locationInfo =
            (mkLocationInfo blacklist locationTags tags) <$> locations
        }
  where
    locationId L.Location {..} = _id
    tagId ET.EntityTag {..} = TagId _TagId
    getF l1 l2 f =
      case f of
        LSTATE    -> L._state l1 == L._state l2
        LCITY     -> L._city l1 == L._city l2
        LDISTRICT -> L._district l1 == L._district l2
        LWARD     -> L._ward l1 == L._ward l2
        LPINCODE  -> L._pincode l1 == L._pincode l2

