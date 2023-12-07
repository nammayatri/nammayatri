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
import Kernel.Utils.Common
import qualified Tools.Maps as Maps

buildOneWaySearchReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DOneWaySearch.OneWaySearchRes ->
  m (BecknReq Search.SearchMessageV2)
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
  where
    getPoints val = val >>= (\routeInfo -> Just routeInfo.points)

buildRentalSearchReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DRentalSearch.RentalSearchRes ->
  m (BecknReq Search.SearchMessageV2)
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
  m (BecknReq Search.SearchMessageV2)
buildSearchReq origin destination searchId _ distance duration customerLanguage disabilityTag merchant _city mbPoints = do
  let transactionId = getId searchId
      messageId = transactionId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildTaxiContext Context.SEARCH messageId (Just transactionId) merchant.bapId bapUrl Nothing Nothing merchant.defaultCity merchant.country False
  let intent = mkIntent origin destination customerLanguage disabilityTag distance duration mbPoints
  let searchMessage = Search.SearchMessageV2 intent

  pure $ BecknReq context searchMessage

mkIntent ::
  DSearchCommon.SearchReqLocation ->
  DSearchCommon.SearchReqLocation ->
  Maybe Maps.Language ->
  Maybe Text ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe [Maps.LatLong] ->
  Search.IntentV2
mkIntent origin destination customerLanguage disabilityTag distance duration mbPoints = do
  let startLocation =
        Search.Stops
          { location = mkLocation origin,
            stopType = Search.START
          }
      endLocation =
        Search.Stops
          { location = mkLocation destination,
            stopType = Search.END
          }

      fulfillment =
        Search.FulfillmentInfoV2
          { stops = [startLocation, endLocation],
            tags =
              if isJust distance || isJust duration
                then Just [mkRouteInfoTags]
                else Nothing,
            customer =
              if isJust customerLanguage || isJust disabilityTag
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
              { code = "route_info",
                name =
                  Just "Route Information",
                short_desc = Nothing
              },
          list =
            [ Search.TagV2
                { display = (\_ -> Just False) =<< distance,
                  descriptor =
                    Just
                      Search.DescriptorV2
                        { code = "distance_info_in_m",
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
                        { code = "duration_info_in_s",
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
                        { code = "route_points",
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
              { code = "customer_info",
                name = Just "Customer Information",
                short_desc = Nothing
              },
          list =
            [ Search.TagV2
                { display = (\_ -> Just False) =<< customerLanguage,
                  descriptor =
                    Just
                      Search.DescriptorV2
                        { code = "customer_language",
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
                        { code = "customer_disability",
                          name = (\_ -> Just "Customer Disability") =<< disabilityTag,
                          short_desc = Nothing
                        },
                  value = (Just . show) =<< disabilityTag
                }
            ]
        }
