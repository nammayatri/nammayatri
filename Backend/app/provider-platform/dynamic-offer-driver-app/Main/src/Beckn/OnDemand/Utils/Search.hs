{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.Search where

import Beckn.ACL.Common (getTagV2)
import Beckn.OnDemand.Utils.Common (firstStop, lastStop)
import qualified BecknV2.OnDemand.Types as Spec
import Control.Lens
import Data.Aeson
import qualified Data.Text as T
import EulerHS.Prelude hiding (id, view, (^?))
import Kernel.External.Maps as Maps
import Kernel.Types.Common

getPickUpLocation :: Spec.SearchReqMessage -> Spec.Location
getPickUpLocation req = do
  let intent = req.searchReqMessageIntent & fromMaybe (error "Missing Intent")
  let fulfillment = intent.intentFulfillment & fromMaybe (error "Missing Fulfillment")
  let stops = fulfillment.fulfillmentStops & fromMaybe (error "Missing Stops")
  let pickUp = firstStop stops & fromMaybe (error "Missing Pickup")
  pickUp.stopLocation & fromMaybe (error "Missing Location")

getDropOffLocation :: Spec.SearchReqMessage -> Spec.Location
getDropOffLocation req = do
  let intent = req.searchReqMessageIntent & fromMaybe (error "Missing Intent")
  let fulfillment = intent.intentFulfillment & fromMaybe (error "Missing Fulfillment")
  let stops = fulfillment.fulfillmentStops & fromMaybe (error "Missing Stops")
  let dropOff = lastStop stops & fromMaybe (error "Missing DropOff")
  dropOff.stopLocation & fromMaybe (error "Missing Location")

getPickUpLocationGps :: Spec.SearchReqMessage -> Text
getPickUpLocationGps req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let stops = fromMaybe (error "Missing Stops") $ fulfillment.fulfillmentStops
  let pickUp = lastStop stops & fromMaybe (error "Missing DropOff")
  let location = pickUp.stopLocation & fromMaybe (error "Missing Location")
  location.locationGps & fromMaybe (error "Missing GPS")

getDropOffLocationGps :: Spec.SearchReqMessage -> Text
getDropOffLocationGps req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let stops = fromMaybe (error "Missing Stops") $ fulfillment.fulfillmentStops
  let dropOff = lastStop stops & fromMaybe (error "Missing DropOff")
  let location = dropOff.stopLocation & fromMaybe (error "Missing Location")
  location.locationGps & fromMaybe (error "Missing GPS")

getDistance :: Spec.SearchReqMessage -> Maybe Meters
getDistance req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let tagGroups = fromMaybe (error "Missing Tags") $ fulfillment.fulfillmentTags
  tagValue <- getTagV2 "route_info" "distance_info_in_m" tagGroups
  distanceValue <- readMaybe $ T.unpack tagValue
  return $ Meters distanceValue

getDuration :: Spec.SearchReqMessage -> Maybe Seconds
getDuration req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let tagGroups = fromMaybe (error "Missing Tags") $ fulfillment.fulfillmentTags
  tagValue <- getTagV2 "route_info" "duration_info_in_s" tagGroups
  durationValue <- readMaybe $ T.unpack tagValue
  Just $ Seconds durationValue

buildCustomerLanguage :: Spec.SearchReqMessage -> Maybe Language
buildCustomerLanguage req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let customer = fromMaybe (error "Missing Customer") $ fulfillment.fulfillmentCustomer
  let customerPerson = fromMaybe (error "Missing Person") $ customer.customerPerson
  let tagGroups = fromMaybe (error "Missing Tags") $ customerPerson.personTags
  tagValue <- getTagV2 "customer_info" "customer_language" tagGroups
  readMaybe $ T.unpack tagValue

buildDisabilityTag :: Spec.SearchReqMessage -> Maybe Text
buildDisabilityTag req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let customer = fromMaybe (error "Missing Customer") $ fulfillment.fulfillmentCustomer
  let customerPerson = fromMaybe (error "Missing Person") $ customer.customerPerson
  let tagGroups = fromMaybe (error "Missing Tags") $ customerPerson.personTags
  tagValue <- getTagV2 "customer_info" "disability_tag" tagGroups
  Just tagValue

buildCustomerPhoneNumber :: Spec.SearchReqMessage -> Maybe Text
buildCustomerPhoneNumber req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let customer = fromMaybe (error "Missing Customer") $ fulfillment.fulfillmentCustomer
  let customerPerson = fromMaybe (error "Missing Person") $ customer.customerPerson
  let tagGroups = fromMaybe (error "Missing Tags") $ customerPerson.personTags
  tagValue <- getTagV2 "customer_info" "customer_phone_number" tagGroups
  readMaybe $ T.unpack tagValue

-- customerPerson <- req ^? (ix "searchReqMessageIntent" . key "intentFulfillment" . key "fulfillmentCustomer" . key "customerPerson" . key "tags") & fromMaybeM (InvalidRequest "Missing Fields")

getIsReallocationEnabled :: Spec.SearchReqMessage -> Maybe Bool
getIsReallocationEnabled req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let tagGroups = fromMaybe (error "Missing Tags") $ fulfillment.fulfillmentTags
  tagValue <- getTagV2 "reallocation_info" "is_reallocation_enabled" tagGroups
  readMaybe $ T.unpack tagValue

buildRoutePoints :: Spec.SearchReqMessage -> Maybe [Maps.LatLong]
buildRoutePoints req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let tagGroups = fromMaybe (error "Missing Tags") $ fulfillment.fulfillmentTags
  getTagV2 "route_info" "route_points" tagGroups >>= decode . encodeUtf8
