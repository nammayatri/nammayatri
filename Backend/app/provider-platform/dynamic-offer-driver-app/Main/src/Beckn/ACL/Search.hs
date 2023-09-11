{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Search where

import Beckn.ACL.Common (getTag)
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified Beckn.Types.Core.Taxi.Search as Search
import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Search as DSearch
import Kernel.External.Maps.Interface (LatLong (..))
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error
import qualified Tools.Maps as Maps

buildSearchReq ::
  (HasFlowEnv m r '["coreVersion" ::: Text]) =>
  Subscriber.Subscriber ->
  Search.SearchReq ->
  m DSearch.DSearchReq
buildSearchReq subscriber req = do
  now <- getCurrentTime
  let context = req.context
  validateContext Context.SEARCH context
  let intent = req.message.intent
  let pickup = intent.fulfillment.start
      dropOff = intent.fulfillment.end
  let distance = getDistance =<< intent.fulfillment.tags
  let duration = getDuration =<< intent.fulfillment.tags
  let customerLanguage = buildCustomerLanguage =<< intent.fulfillment.customer
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  let disabilityTag = buildDisabilityTag =<< intent.fulfillment.customer
  let messageId = context.message_id
  transactionId <- context.transaction_id & fromMaybeM (InvalidRequest "Missing transaction_id")
  pure
    DSearch.DSearchReq
      { messageId = messageId,
        transactionId = transactionId,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        bapCity = context.city,
        bapCountry = context.country,
        pickupLocation = LatLong {lat = pickup.location.gps.lat, lon = pickup.location.gps.lon},
        pickupTime = now,
        dropLocation = LatLong {lat = dropOff.location.gps.lat, lon = dropOff.location.gps.lon},
        pickupAddress = pickup.location.address,
        dropAddrress = dropOff.location.address,
        routeDistance = distance,
        routeDuration = duration,
        device = Nothing,
        routePoints = buildRoutePoints =<< intent.fulfillment.tags, --------TODO------Take proper input---------
        customerLanguage = customerLanguage,
        disabilityTag = disabilityTag
      }

getDistance :: Search.TagGroups -> Maybe Meters
getDistance tagGroups = do
  tagValue <- getTag "route_info" "distance_info_in_m" tagGroups
  distanceValue <- readMaybe $ T.unpack tagValue
  Just $ Meters distanceValue

getDuration :: Search.TagGroups -> Maybe Seconds
getDuration tagGroups = do
  tagValue <- getTag "route_info" "duration_info_in_s" tagGroups
  durationValue <- readMaybe $ T.unpack tagValue
  Just $ Seconds durationValue

buildCustomerLanguage :: Search.Customer -> Maybe Language
buildCustomerLanguage Search.Customer {..} = do
  tagValue <- getTag "customer_info" "customer_language" person.tags
  readMaybe $ T.unpack tagValue

buildDisabilityTag :: Search.Customer -> Maybe Text
buildDisabilityTag Search.Customer {..} = do
  tagValue <- getTag "customer_info" "customer_disability" person.tags
  readMaybe $ T.unpack tagValue

buildRoutePoints :: Search.TagGroups -> Maybe [Maps.LatLong]
buildRoutePoints tagGroups = do
  tagValue <- getTag "route_info" "route_points" tagGroups
  decode $ encodeUtf8 tagValue
