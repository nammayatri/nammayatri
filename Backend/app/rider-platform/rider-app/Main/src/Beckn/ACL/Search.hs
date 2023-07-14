{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Search (buildRentalSearchReq, buildOneWaySearchReq) where

import Beckn.ACL.Common (mkLocation)
import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified Data.Text as T
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Action.UI.Search.Rental as DRentalSearch
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSearchReq
import EulerHS.Prelude hiding (state)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Tools.Maps as Maps

buildOneWaySearchReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DOneWaySearch.OneWaySearchRes ->
  m (BecknReq Search.SearchMessage)
buildOneWaySearchReq DOneWaySearch.OneWaySearchRes {..} =
  buildSearchReq
    origin
    destination
    searchId
    device
    (shortestRouteInfo >>= (.distance))
    (shortestRouteInfo >>= (.duration))
    customerLanguage
    merchant
    (getPoints shortestRouteInfo)
  where
    getPoints val = val >>= (\routeInfo -> Just routeInfo.points)

buildRentalSearchReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DRentalSearch.RentalSearchRes ->
  m (BecknReq Search.SearchMessage)
buildRentalSearchReq DRentalSearch.RentalSearchRes {..} =
  buildSearchReq
    origin
    origin
    searchId
    Nothing
    Nothing
    Nothing
    Nothing
    merchant
    Nothing

buildSearchReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DSearchCommon.SearchReqLocation ->
  DSearchCommon.SearchReqLocation ->
  Id DSearchReq.SearchRequest ->
  Maybe Text ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe Maps.Language ->
  DM.Merchant ->
  Maybe [Maps.LatLong] ->
  m (BecknReq Search.SearchMessage)
buildSearchReq origin destination searchId _ distance duration customerLanguage merchant points = do
  let transactionId = getId searchId
      messageId = transactionId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/cab/v1/" <> T.unpack merchant.id.getId)
  context <- buildTaxiContext Context.SEARCH messageId (Just transactionId) merchant.bapId bapUrl Nothing Nothing merchant.city merchant.country False
  let intent = mkIntent origin destination customerLanguage distance duration
  -- let mbRouteInfo = Search.RouteInfo {distance, duration, points}
  let searchMessage = Search.SearchMessage intent

  pure $ BecknReq context searchMessage

mkIntent ::
  DSearchCommon.SearchReqLocation ->
  DSearchCommon.SearchReqLocation ->
  Maybe Maps.Language ->
  Maybe Meters ->
  Maybe Seconds ->
  Search.Intent
mkIntent origin destination customerLanguage distance duration = do
  let startLocation =
        Search.StartInfo
          { location = mkLocation origin
          }
      endLocation =
        Search.StopInfo
          { location = mkLocation destination
          }
      -- endLocation = mkStopInfo

      fulfillment =
        Search.FulfillmentInfo
          { start = startLocation,
            end = endLocation,
            tags =
              if isJust distance || isJust duration
                then
                  Just $
                    Search.TG
                      [ mkRouteInfoTags
                      ]
                else -- Search.Tags
                --   { --customer_language = customerLanguage
                --     code = "route_info",
                --     name = "Route Information",
                --     list_1_code = maybe Nothing (\_ -> Just "distance_info_in_m") distance,
                --     list_1_name = maybe Nothing (\_ -> Just "Distance Information In Meters") distance, --"Distance Information In Meters",
                --     list_1_value = maybe Nothing (\distanceInM -> Just $ show distanceInM.getMeters) distance,
                --     list_2_code = maybe Nothing (\_ -> Just "duration_info_in_s") duration, --"duration_info_in_s",
                --     list_2_name = maybe Nothing (\_ -> Just "Duration Information In Seconds") duration, --"Duration Information In Seconds",
                --     list_2_value = maybe Nothing (\durationInS -> Just $ show durationInS.getSeconds) duration
                --   }
                  Nothing,
            customer =
              if isJust customerLanguage
                then
                  Just $
                    Search.Customer
                      { person =
                          Search.Person
                            { tags = Search.TG [mkCustomerInfoTags]
                            -- Search.Tags
                            --   { --customer_language = customerLanguage
                            --     code = "customer_info",
                            --     name = "Customer Information",
                            --     list_1_code = maybe Nothing (\_ -> Just "customer_language") customerLanguage,
                            --     list_1_name = maybe Nothing (\_ -> Just "Customer Language") customerLanguage, --"Distance Information In Meters",
                            --     list_1_value = maybe Nothing (\language -> Just $ show language) customerLanguage,
                            --     list_2_code = Nothing,
                            --     list_2_name = Nothing,
                            --     list_2_value = Nothing
                            --   }
                            }
                      }
                else Nothing
          }
  Search.Intent
    { ..
    }
  where
    -- mkLocation info =
    --   Search.Location
    --     { gps =
    --         Search.Gps
    --           { lat = info.gps.lat,
    --             lon = info.gps.lon
    --           },
    --       address =
    --         Just
    --           Search.Address
    --             { locality = info.address.area,
    --               state = info.address.state,
    --               country = info.address.country,
    --               building = info.address.building,
    --               street = info.address.street,
    --               city = info.address.city,
    --               area_code = info.address.areaCode,
    --               door = info.address.door,
    --               ward = info.address.ward
    --             }
    --     }
    mkRouteInfoTags =
      Search.TagGroup
        { display = False,
          code = "route_info",
          name = "Route Information",
          list =
            [ Search.Tag
                { display = (\_ -> Just False) =<< distance,
                  code = (\_ -> Just "distance_info_in_m") =<< distance,
                  name = (\_ -> Just "Distance Information In Meters") =<< distance,
                  value = (\distanceInM -> Just $ show distanceInM.getMeters) =<< distance
                },
              Search.Tag
                { display = (\_ -> Just False) =<< duration,
                  code = (\_ -> Just "duration_info_in_s") =<< duration,
                  name = (\_ -> Just "Duration Information In Seconds") =<< duration,
                  value = (\durationInS -> Just $ show durationInS.getSeconds) =<< duration
                }
            ]
        }

    mkCustomerInfoTags =
      Search.TagGroup
        { display = False,
          code = "customer_info",
          name = "Customer Information",
          list =
            [ Search.Tag
                { display = (\_ -> Just False) =<< customerLanguage,
                  code = (\_ -> Just "customer_language") =<< customerLanguage,
                  name = (\_ -> Just "Customer Language") =<< customerLanguage,
                  value = (Just . show) =<< customerLanguage
                }
            ]
        }

-- data TagGroup = TagGroup
--   { display :: Bool,
--     code :: String,
--     name :: String,
--     list :: [Tag]
--   }
--   deriving (Generic, Show, ToSchema)

-- data Tag = Tag
--   { display :: Bool,
--     code :: String,
--     name :: String,
--     value :: String
--   }
-- variant = Common.castVariant estimate.vehicleVariant
-- vehicle = OS.FulfillmentVehicle {category = variant}
