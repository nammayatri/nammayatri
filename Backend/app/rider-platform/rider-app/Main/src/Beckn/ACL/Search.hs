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
import Data.Aeson (encode)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TE
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Action.UI.Search.Rental as DRentalSearch
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantConfigNew as DMC
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
    disabilityTag
    merchant
    merchantConfig
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
    Nothing
    merchant
    merchantConfig
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
  DMC.MerchantConfigNew ->
  Maybe [Maps.LatLong] ->
  m (BecknReq Search.SearchMessage)
buildSearchReq origin destination searchId _ distance duration customerLanguage disabilityTag merchant merchantConfig mbPoints = do
  let transactionId = getId searchId
      messageId = transactionId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  context <- buildTaxiContext Context.SEARCH messageId (Just transactionId) merchant.bapId bapUrl Nothing Nothing merchantConfig.city merchantConfig.country False
  let intent = mkIntent origin destination customerLanguage disabilityTag distance duration mbPoints
  let searchMessage = Search.SearchMessage intent

  pure $ BecknReq context searchMessage

mkIntent ::
  DSearchCommon.SearchReqLocation ->
  DSearchCommon.SearchReqLocation ->
  Maybe Maps.Language ->
  Maybe Text ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe [Maps.LatLong] ->
  Search.Intent
mkIntent origin destination customerLanguage disabilityTag distance duration mbPoints = do
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
                      [ mkRouteInfoTags
                      ]
                else Nothing,
            customer =
              if isJust customerLanguage || isJust disabilityTag
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
