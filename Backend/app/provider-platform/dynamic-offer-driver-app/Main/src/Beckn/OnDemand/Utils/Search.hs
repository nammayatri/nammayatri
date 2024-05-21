{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.Search where

import Beckn.OnDemand.Utils.Common (firstStop, lastStop)
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.Utils as Utils
import Control.Lens
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Time
import EulerHS.Prelude hiding (id, view, (^?))
import Kernel.External.Maps as Maps
import Kernel.Types.Common
import Kernel.Utils.Common (fromMaybeM)
import Tools.Error (GenericError (InvalidRequest))

getPickUpTime :: Spec.SearchReqMessage -> Maybe Data.Time.UTCTime
getPickUpTime req =
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= firstStop
    >>= (.stopTime)
    >>= (.timeTimestamp)

getPickUpLocation :: MonadFlow m => Spec.SearchReqMessage -> m Spec.Location
getPickUpLocation req =
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= firstStop
    >>= (.stopLocation)
    & fromMaybeM (InvalidRequest "Missing Pickup Location")

getDropOffLocation :: Spec.SearchReqMessage -> Maybe Spec.Location
getDropOffLocation req = do
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= lastStop
    >>= (.stopLocation)

getPickUpLocationGps :: MonadFlow m => Spec.SearchReqMessage -> m Text
getPickUpLocationGps req =
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= firstStop
    >>= (.stopLocation)
    >>= (.locationGps)
    & fromMaybeM (InvalidRequest "Missing Pickup Location GPS")

getDropOffLocationGps :: Spec.SearchReqMessage -> Maybe Text
getDropOffLocationGps req = do
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= lastStop
    >>= (.stopLocation)
    >>= (.locationGps)

getDistance :: Spec.SearchReqMessage -> Maybe Meters
getDistance req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  let tagValue = Utils.getTagV2 Tag.ROUTE_INFO Tag.DISTANCE_INFO_IN_M tagGroups
  Just . Meters =<< readMaybe . T.unpack =<< tagValue

getDuration :: Spec.SearchReqMessage -> Maybe Seconds
getDuration req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  let tagValue = Utils.getTagV2 Tag.ROUTE_INFO Tag.DURATION_INFO_IN_S tagGroups
  Just . Seconds =<< readMaybe . T.unpack =<< tagValue

getReturnTime :: Spec.SearchReqMessage -> Maybe Data.Time.UTCTime
getReturnTime req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  let tagValue = Utils.getTagV2 Tag.ROUTE_INFO Tag.RETURN_TIME tagGroups
  readMaybe . T.unpack =<< tagValue

getRoundTrip :: Spec.SearchReqMessage -> Maybe Bool
getRoundTrip req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  let tagValue = Utils.getTagV2 Tag.ROUTE_INFO Tag.ROUND_TRIP tagGroups
  readMaybe . T.unpack =<< tagValue

buildCustomerLanguage :: Spec.SearchReqMessage -> Maybe Language
buildCustomerLanguage req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentCustomer) >>= (.customerPerson) >>= (.personTags)
  let tagValue = Utils.getTagV2 Tag.CUSTOMER_INFO Tag.CUSTOMER_LANGUAGE tagGroups
  readMaybe . T.unpack =<< tagValue

checkIfDashboardSearch :: Spec.SearchReqMessage -> Maybe Bool
checkIfDashboardSearch req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentCustomer) >>= (.customerPerson) >>= (.personTags)
  let tagValue = Utils.getTagV2 Tag.CUSTOMER_INFO Tag.DASHBOARD_USER tagGroups
  readMaybe . T.unpack =<< tagValue

buildDisabilityTag :: Spec.SearchReqMessage -> Maybe Text
buildDisabilityTag req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentCustomer) >>= (.customerPerson) >>= (.personTags)
  Utils.getTagV2 Tag.CUSTOMER_INFO Tag.CUSTOMER_DISABILITY tagGroups

buildCustomerPhoneNumber :: Spec.SearchReqMessage -> Maybe Text
buildCustomerPhoneNumber req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentCustomer) >>= (.customerPerson) >>= (.personTags)
  Utils.getTagV2 Tag.CUSTOMER_INFO Tag.CUSTOMER_PHONE_NUMBER tagGroups

-- customerPerson <- req ^? (ix "searchReqMessageIntent" . key "intentFulfillment" . key "fulfillmentCustomer" . key "customerPerson" . key "tags") & fromMaybeM (InvalidRequest "Missing Fields")

getIsReallocationEnabled :: Spec.SearchReqMessage -> Maybe Bool
getIsReallocationEnabled req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
      tagValue = Utils.getTagV2 Tag.REALLOCATION_INFO Tag.IS_REALLOCATION_ENABLED tagGroups
  readMaybe . T.unpack =<< tagValue

buildRoutePoints :: Spec.SearchReqMessage -> Maybe [Maps.LatLong]
buildRoutePoints req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  decode . encodeUtf8 =<< Utils.getTagV2 Tag.ROUTE_INFO Tag.WAYPOINTS tagGroups

buildMultipleRoutesTag :: Spec.SearchReqMessage -> Maybe [Maps.RouteInfo]
buildMultipleRoutesTag req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  decode . encodeUtf8 =<< Utils.getTagV2 Tag.ROUTE_INFO Tag.MULTIPLE_ROUTES tagGroups
