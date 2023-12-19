{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Search (buildRentalSearchReq, buildRentalSearchReqV2, buildOneWaySearchReq, buildOneWaySearchReqV2) where

import Beckn.ACL.Common (mkLocation)
import qualified Beckn.Types.Core.Taxi.Search as Search
import Control.Lens ((%~))
import Data.Aeson (encode)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TE
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Action.UI.Search.Rental as DRentalSearch
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSearchReq
import EulerHS.Prelude hiding (state, (%~))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common hiding (mkLocation)
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
    disabilityTag
    merchant
    city
    (getPoints shortestRouteInfo)
    phoneNumber
    isReallocationEnabled
  where
    getPoints val = val >>= (\routeInfo -> Just routeInfo.points)

buildOneWaySearchReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DOneWaySearch.OneWaySearchRes ->
  m (BecknReqV2 Search.SearchMessageV2)
buildOneWaySearchReqV2 DOneWaySearch.OneWaySearchRes {..} =
  buildSearchReqV2
    origin
    destination
    searchId
    device
    (shortestRouteInfo >>= (.distance))
    (shortestRouteInfo >>= (.duration))
    customerLanguage
    disabilityTag
    merchant
    city
    (getPoints shortestRouteInfo)
    phoneNumber
    isReallocationEnabled
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
    Nothing
    merchant
    city
    Nothing
    phoneNumber
    Nothing

buildRentalSearchReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DRentalSearch.RentalSearchRes ->
  m (BecknReqV2 Search.SearchMessageV2)
buildRentalSearchReqV2 DRentalSearch.RentalSearchRes {..} =
  buildSearchReqV2
    origin
    origin
    searchId
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    merchant
    city
    Nothing
    phoneNumber
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
  Maybe Text ->
  DM.Merchant ->
  Context.City ->
  Maybe [Maps.LatLong] ->
  Maybe Text ->
  Maybe Bool ->
  m (BecknReq Search.SearchMessage)
buildSearchReq origin destination searchId _ distance duration customerLanguage disabilityTag merchant _city mbPoints mbPhoneNumber mbIsReallocationEnabled = do
  let transactionId = getId searchId
      messageId = transactionId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildTaxiContext Context.SEARCH messageId (Just transactionId) merchant.bapId bapUrl Nothing Nothing merchant.defaultCity merchant.country False
  let intent = mkIntent origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber mbIsReallocationEnabled
  let searchMessage = Search.SearchMessage intent

  pure $ BecknReq context searchMessage

buildSearchReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DSearchCommon.SearchReqLocation ->
  DSearchCommon.SearchReqLocation ->
  Id DSearchReq.SearchRequest ->
  Maybe Text ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe Maps.Language ->
  Maybe Text ->
  DM.Merchant ->
  Context.City ->
  Maybe [Maps.LatLong] ->
  Maybe Text ->
  Maybe Bool ->
  m (BecknReqV2 Search.SearchMessageV2)
buildSearchReqV2 origin destination searchId _ distance duration customerLanguage disabilityTag merchant _city mbPoints mbPhoneNumber mbIsReallocationEnabled = do
  let transactionId = getId searchId
      messageId = transactionId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildTaxiContextV2 Context.SEARCH messageId (Just transactionId) merchant.bapId bapUrl Nothing Nothing merchant.defaultCity merchant.country
  let intent = mkIntentV2 origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber mbIsReallocationEnabled
  let searchMessage = Search.SearchMessageV2 intent

  pure $ BecknReqV2 context searchMessage

mkIntent ::
  DSearchCommon.SearchReqLocation ->
  DSearchCommon.SearchReqLocation ->
  Maybe Maps.Language ->
  Maybe Text ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe [Maps.LatLong] ->
  Maybe Text ->
  Maybe Bool ->
  Search.Intent
mkIntent origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber mbIsReallocationEnabled = do
  let startLocation =
        Search.StartInfo
          { location = mkLocation origin
          }
      endLocation =
        Search.StopInfo
          { location = mkLocation destination
          }

      fulfillment =
        Search.FulfillmentInfo
          { start = startLocation,
            end = endLocation,
            tags =
              if isJust distance || isJust duration
                then
                  Just $
                    Search.TG
                      [ mkRouteInfoTags,
                        mkReallocationInfoTags
                      ]
                else Nothing,
            customer =
              if isJust customerLanguage || isJust disabilityTag || isJust mbPhoneNumber
                then
                  Just $
                    Search.Customer
                      { person =
                          Search.Person
                            { tags = Search.TG [mkCustomerInfoTags]
                            }
                      }
                else Nothing
          }
  Search.Intent
    { ..
    }
  where
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
                },
              Search.Tag
                { display = (\_ -> Just False) =<< mbPoints,
                  code = (\_ -> Just "route_points") =<< mbPoints,
                  name = (\_ -> Just "Route Points") =<< mbPoints,
                  value = LT.toStrict . TE.decodeUtf8 . encode <$> mbPoints
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
                },
              Search.Tag
                { display = (\_ -> Just False) =<< disabilityTag,
                  code = (\_ -> Just "customer_disability") =<< disabilityTag,
                  name = (\_ -> Just "Customer Disability") =<< disabilityTag,
                  value = (Just . show) =<< disabilityTag
                }
            ]
        }

    mkReallocationInfoTags =
      Search.TagGroup
        { display = False,
          code = "reallocation_info",
          name = "Reallocation Information",
          list =
            [ Search.Tag
                { display = (\_ -> Just False) =<< mbIsReallocationEnabled,
                  code = (\_ -> Just "is_reallocation_enabled") =<< mbIsReallocationEnabled,
                  name = (\_ -> Just "Is Reallocation Enabled") =<< mbIsReallocationEnabled,
                  value = (Just . show) =<< mbIsReallocationEnabled
                }
            ]
        }

mkIntentV2 ::
  DSearchCommon.SearchReqLocation ->
  DSearchCommon.SearchReqLocation ->
  Maybe Maps.Language ->
  Maybe Text ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe [Maps.LatLong] ->
  Maybe Text ->
  Maybe Bool ->
  Search.IntentV2
mkIntentV2 origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber mbIsReallocationEnabled = do
  let startLocation =
        Search.Stop
          { location = mkLocation origin,
            stopType = Search.START
          }
      endLocation =
        Search.Stop
          { location = mkLocation destination,
            stopType = Search.END
          }

      fulfillment =
        Search.FulfillmentInfoV2
          { stops = [startLocation, endLocation],
            tags =
              if isJust distance || isJust duration
                then Just [mkRouteInfoTags, mkReallocationInfoTags]
                else Nothing,
            customer =
              if isJust customerLanguage || isJust disabilityTag || isJust mbPhoneNumber
                then
                  Just $
                    Search.CustomerV2
                      { person =
                          Search.PersonV2
                            { tags = [mkCustomerInfoTags]
                            }
                      }
                else Nothing
          }

      payment =
        Search.Payment
          { tags = Nothing
          }

  Search.IntentV2
    { ..
    }
  where
    mkRouteInfoTags =
      Search.TagGroupV2
        { display = False,
          descriptor =
            Search.DescriptorV2
              { code = Just "route_info",
                name = Just "Route Information",
                short_desc = Nothing
              },
          list =
            [ Search.TagV2
                { display = (\_ -> Just False) =<< distance,
                  descriptor =
                    Just
                      Search.DescriptorV2
                        { code = (\_ -> Just "distance_info_in_m") =<< distance,
                          name = (\_ -> Just "Distance Information In Meters") =<< distance,
                          short_desc = Nothing
                        },
                  value = (\distanceInM -> Just $ show distanceInM.getMeters) =<< distance
                },
              Search.TagV2
                { display = (\_ -> Just False) =<< duration,
                  descriptor =
                    Just
                      Search.DescriptorV2
                        { code = (\_ -> Just "duration_info_in_s") =<< duration,
                          name = (\_ -> Just "Duration Information In Seconds") =<< duration,
                          short_desc = Nothing
                        },
                  value = (\durationInS -> Just $ show durationInS.getSeconds) =<< duration
                },
              Search.TagV2
                { display = (\_ -> Just False) =<< mbPoints,
                  descriptor =
                    Just
                      Search.DescriptorV2
                        { code = (\_ -> Just "route_points") =<< mbPoints,
                          name = (\_ -> Just "Route Points") =<< mbPoints,
                          short_desc = Nothing
                        },
                  value = LT.toStrict . TE.decodeUtf8 . encode <$> mbPoints
                }
            ]
        }

    mkCustomerInfoTags =
      Search.TagGroupV2
        { display = False,
          descriptor =
            Search.DescriptorV2
              { code = Just "customer_info",
                name = Just "Customer Information",
                short_desc = Nothing
              },
          list =
            [ Search.TagV2
                { display = (\_ -> Just False) =<< customerLanguage,
                  descriptor =
                    Just
                      Search.DescriptorV2
                        { code = (\_ -> Just "customer_language") =<< customerLanguage,
                          name = (\_ -> Just "Customer Language") =<< customerLanguage,
                          short_desc = Nothing
                        },
                  value = (Just . show) =<< customerLanguage
                },
              Search.TagV2
                { display = (\_ -> Just False) =<< disabilityTag,
                  descriptor =
                    Just
                      Search.DescriptorV2
                        { code = (\_ -> Just "customer_disability") =<< disabilityTag,
                          name = (\_ -> Just "Customer Disability") =<< disabilityTag,
                          short_desc = Nothing
                        },
                  value = (Just . show) =<< disabilityTag
                },
              Search.TagV2
                { display = (\_ -> Just False) =<< mbPhoneNumber,
                  descriptor =
                    Just
                      Search.DescriptorV2
                        { code = (\_ -> Just "customer_phone_number") =<< mbPhoneNumber,
                          name = (\_ -> Just "Customer Phone Number") =<< mbPhoneNumber,
                          short_desc = Nothing
                        },
                  value = (Just . show) =<< mbPhoneNumber
                }
            ]
        }

    mkReallocationInfoTags =
      Search.TagGroupV2
        { display = False,
          descriptor =
            Search.DescriptorV2
              { code = Just "reallocation_info",
                name = Just "Reallocation Information",
                short_desc = Nothing
              },
          list =
            [ Search.TagV2
                { display = (\_ -> Just False) =<< mbIsReallocationEnabled,
                  descriptor =
                    Just $
                      Search.DescriptorV2
                        { code = (\_ -> Just "is_reallocation_enabled") =<< mbIsReallocationEnabled,
                          name = (\_ -> Just "Is Reallocation Enabled") =<< mbIsReallocationEnabled,
                          short_desc = Nothing
                        },
                  value = (Just . show) =<< mbIsReallocationEnabled
                }
            ]
        }
