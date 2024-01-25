{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

-- shrey00 : this is the reference file

module Beckn.ACL.Search
  ( buildSearchReqV1,
    buildSearchReqV2,
  )
where

import Beckn.ACL.Common (mkLocation)
import qualified Beckn.OnDemand.Transformer.Search as Search
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified BecknV2.OnDemand.Types as Spec
import Control.Lens ((%~))
import Data.Aeson (encode)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TE
import qualified Domain.Action.UI.Search as DSearch
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSearchReq
import EulerHS.Prelude hiding (state, (%~))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Tools.Maps as Maps

buildSearchReqV1 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DSearch.SearchRes ->
  m (BecknReq Search.SearchMessage)
buildSearchReqV1 DSearch.SearchRes {..} =
  buildSearchReq
    origin
    stops
    searchId
    device
    distance
    duration
    customerLanguage
    disabilityTag
    merchant
    city
    (getPoints shortestRouteInfo)
    phoneNumber
    isReallocationEnabled
  where
    getPoints val = val >>= (\routeInfo -> Just routeInfo.points)

buildSearchReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DSearch.SearchRes ->
  m (Spec.SearchReq)
buildSearchReqV2 DSearch.SearchRes {..} = do
  bapUri <- Utils.mkBapUri merchant.id
  Search.buildBecknSearchReqV2
    Context.SEARCH
    Context.MOBILITY
    origin
    stops
    searchId
    distance
    duration
    customerLanguage
    disabilityTag
    merchant
    bapUri
    (getPoints shortestRouteInfo)
    phoneNumber
    isReallocationEnabled
  where
    getPoints val = val >>= (\routeInfo -> Just routeInfo.points)

buildSearchReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DSearch.SearchReqLocation ->
  [DSearch.SearchReqLocation] ->
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
buildSearchReq origin stops searchId _ distance duration customerLanguage disabilityTag merchant _city mbPoints mbPhoneNumber mbIsReallocationEnabled = do
  let transactionId = getId searchId
      messageId = transactionId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildTaxiContext Context.SEARCH messageId (Just transactionId) merchant.bapId bapUrl Nothing Nothing _city merchant.country False
  let intent = mkIntent origin stops customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber mbIsReallocationEnabled
  let searchMessage = Search.SearchMessage intent

  pure $ BecknReq context searchMessage

mkIntent ::
  DSearch.SearchReqLocation ->
  [DSearch.SearchReqLocation] ->
  Maybe Maps.Language ->
  Maybe Text ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe [Maps.LatLong] ->
  Maybe Text ->
  Maybe Bool ->
  Search.Intent
mkIntent origin stops customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber mbIsReallocationEnabled = do
  let mbDestination = listToMaybe stops
      startLocation =
        Search.StartInfo
          { location = mkLocation origin
          }
      mbEndLocation =
        ( \destination ->
            Search.StopInfo
              { location = mkLocation destination
              }
        )
          <$> mbDestination

      fulfillment =
        Search.FulfillmentInfo
          { start = startLocation,
            end = mbEndLocation,
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
                },
              Search.Tag
                { display = (\_ -> Just False) =<< mbPhoneNumber,
                  code = (\_ -> Just "customer_phone_number") =<< mbPhoneNumber,
                  name = (\_ -> Just "Customer Phone Number") =<< mbPhoneNumber,
                  value = (Just . show) =<< mbPhoneNumber
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
