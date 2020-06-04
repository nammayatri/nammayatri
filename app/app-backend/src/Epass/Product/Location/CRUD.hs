module Epass.Product.Location.CRUD where

import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import Data.Aeson
import qualified Data.List as List
import Data.Map.Strict
import qualified Epass.Data.Accessor as Accessor
import Epass.Product.Common
import qualified Epass.Storage.Queries.Blacklist as DB
import qualified Epass.Storage.Queries.EntityTag as ETag
import qualified Epass.Storage.Queries.Location as DB
import qualified Epass.Storage.Queries.PassApplication as DB
import qualified Epass.Storage.Queries.Tag as Tag
import Epass.Types.API.Location.CRUD
import Epass.Types.API.PassApplication
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Common as Location (Location (..))
import Epass.Types.Storage.Blacklist as BL
import qualified Epass.Types.Storage.EntityTag as ET
import Epass.Types.Storage.Location as L
import Epass.Types.Storage.PassApplication
import Epass.Types.Storage.Tag as T
import Epass.Utils.Routes
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant

-- WIP
listLocation ::
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  LocateBy -> -- distinct
  LocateBy -> -- filterby type
  Text -> -- filterby value
  FlowHandler ListLocationRes
listLocation regToken offset limit distinctBy filterByT filterBy =
  withFlowHandler $ do
    verifyToken regToken
    -- :TODO move the nubBy function to beam (or check nub suits )
    locations <-
      List.nubBy (\l1 l2 -> getF l1 l2 distinctBy)
        <$> (DB.findByStOrDistrict offset limit filterByT filterBy)
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
        LSTATE -> L._state l1 == L._state l2
        LCITY -> L._city l1 == L._city l2
        LDISTRICT -> L._district l1 == L._district l2
        LWARD -> L._ward l1 == L._ward l2
        LPINCODE -> L._pincode l1 == L._pincode l2
