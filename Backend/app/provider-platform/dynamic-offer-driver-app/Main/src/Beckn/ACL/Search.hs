{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Search where

import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified Beckn.Types.Core.Taxi.Search as Search
-- import qualified Data.Text as T
import qualified Domain.Action.Beckn.Search as DSearch
import Kernel.External.Maps.Interface (LatLong (..))
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error

buildSearchReq ::
  (HasFlowEnv m r '["coreVersion" ::: Text]) =>
  Subscriber.Subscriber ->
  Search.SearchReq ->
  m DSearch.DSearchReq
buildSearchReq subscriber req = do
  let context = req.context
  validateContext Context.SEARCH context
  let intent = req.message.intent
  let pickup = intent.fulfillment.start
      dropOff = intent.fulfillment.end
  let distance = maybe Nothing (getDistance) intent.fulfillment.tags
  let duration = maybe Nothing (getDuration) intent.fulfillment.tags
  -- let (distance, duration) = maybe (Nothing, Nothing) (buildDistanceAndDuration) intent.fulfillment.tags
  let customerLanguage = maybe Nothing (buildCustomerLanguage) intent.fulfillment.customer
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
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
        pickupTime = pickup.time.timestamp,
        dropLocation = LatLong {lat = dropOff.location.gps.lat, lon = dropOff.location.gps.lon},
        pickupAddress = pickup.location.address,
        dropAddrress = dropOff.location.address,
        routeDistance = distance,
        routeDuration = duration,
        device = Nothing,
        customerLanguage = customerLanguage --intent.fulfillment.tags.customer_language
      }

-- buildDistanceAndDuration :: [Search.TagGroup] -> (Maybe Meters, Maybe Seconds)
-- buildDistanceAndDuration tags = do
--   tagGroup <- find (\tagGroup -> tagGroup.code == "route_info") tags
--   if tags.code == "route_info"
--     then
--       let distance = getDistance tagGroup
--           duration = getDuration tagGroup
--        in (distance, duration)
--     else (Nothing, Nothing)

getDistance :: [Search.TagGroup] -> Maybe Meters
getDistance tagGroups = do
  tagGroup <- find (\tagGroup -> tagGroup.code == "route_info") tagGroups
  tag <- find (\tag -> tag.code == Just "distance_info_in_m") tagGroup.list
  tagValue <- tag.value
  distanceValue <- readMaybe tagValue
  Just $ Meters distanceValue

-- if list1Code == "distance_info_in_m"
--   then do
--     list1Value <- tags.list_1_value
--     distanceValue <- readMaybe $ T.unpack list1Value
--     Just $ Meters distanceValue
--   else Nothing

getDuration :: [Search.TagGroup] -> Maybe Seconds
getDuration tagGroups = do
  tagGroup <- find (\tagGroup -> tagGroup.code == "route_info") tagGroups
  tag <- find (\tag -> tag.code == Just "duration_info_in_s") tagGroup.list
  tagValue <- tag.value
  durationValue <- readMaybe tagValue
  Just $ Seconds durationValue

-- list2Code <- tags.list_2_code
-- if list2Code == "duration_info_in_s"
--   then do
--     list2Value <- tags.list_2_value
--     durationValue <- readMaybe $ T.unpack list2Value
--     Just $ Seconds durationValue
--   else -- readMaybe list2Value
--     Nothing

buildCustomerLanguage :: Search.Customer -> Maybe Language
buildCustomerLanguage Search.Customer {..} = do
  tagGroup <- find (\tagGroup -> tagGroup.code == "customer_info") person.tags
  tag <- find (\tag -> tag.code == Just "customer_language") tagGroup.list
  tagValue <- tag.value
  readMaybe tagValue

-- list1Code <- tags.list_1_code
-- if tags.code == "customer_info" && list1Code == "customer_language"
--   then do
--     list1Value <- tags.list_1_value
--     readMaybe $ T.unpack list1Value
--   else -- Just $ Seconds durationValue
--   -- readMaybe list2Value
--     Nothing

-- when isRouteTag $ do
--   distance <- if isDistanceTag then
--     maybe Nothing (\metersL -> do
--       maybe Nothing
--       ) list_1_value
--     whenJust list_1_value $ \meters -> do
--       whenJust (readMaybe meters) $ \distanceInt -> Just $ Meters distanceInt
--       return Nothing
--     return Nothing
--   -- case (isDistanceTag,isDurationTag) of
--   --   (True, True) ->  do
--   --     distance <- do
--   --       whenJust list_1_value $ \meters -> do
--   --         whenJust (readMaybe meters) $ \distanceInt -> Just $ Meters distanceInt
--   --       Nothing
--   --     duration <- do
--   --       whenJust list_2_value $ \seconds -> do
--   --         whenJust (readMaybe seconds) $ \durationInt -> Seconds durationInt
--   --       Nothing
--   --   (True,False) -> do
--     distance <- do
--       whenJust list_1_value $ \meters -> do
--         whenJust (readMaybe meters) $ \distanceInt -> Just $ Meters distanceInt
--       Nothing
--     return (distance, Nothing)
--   (False,True) -> do
--     duration <- do
--       whenJust list_2_value $ \seconds -> do
--         whenJust (readMaybe seconds) $ \durationInt -> Seconds durationInt
--       Nothing
--     return (Nothing,duration)
--     _,_ -> Nothing

-- return (Nothing,Nothing)

-- where isRouteTag = tag.code == "route_info"
--       isDistanceTag = tag.list_1_code == Just "distance_info_in_m" && isJust list_1_value
--       isDurationTag = tag.list_2_code == Just "duration_info_in_s" && isJust list_2_value

-- code = "route_info",--------TagGroup
-- -- name = "Route Information",
-- list_1_code = maybe Nothing (\_ -> Just "distance_info_in_m") distance,
-- -- list_1_name = maybe Nothing (\_ -> Just "Distance Information In Meters") distance, --"Distance Information In Meters",
-- list_1_value = maybe Nothing (\distanceInM -> Just $ show distanceInM.getMeters) distance,
-- list_2_code = maybe Nothing (\_ -> Just "duration_info_in_s") duration, --"duration_info_in_s",
-- -- list_2_name = maybe Nothing (\_ -> Just "Duration Information In Seconds") duration, --"Duration Information In Seconds",
-- list_2_value = maybe Nothing (\durationInS -> Just $ show durationInS.getSeconds) duration
