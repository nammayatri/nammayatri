{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.OnDemand.Utils.Common where

import qualified BecknV2.OnDemand.Types as Spec
import Control.Lens ((%~))
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Types.Merchant as DM
import EulerHS.Prelude hiding (id, (%~))
import qualified Kernel.Prelude as KP
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common

mkBapUri :: (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) => Id DM.Merchant -> m KP.BaseUrl
mkBapUri merchantId = asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchantId.getId)

showContextAction :: Context.Action -> Maybe Text
showContextAction = A.decode . A.encode

showContextDomain :: Context.Domain -> Maybe Text
showContextDomain = A.decode . A.encode

buildContextLocation :: Context.City -> Context.Country -> Maybe Spec.Location
buildContextLocation city country = do
  let specCityCode = A.decode $ A.encode city
      specCountryCode = A.decode $ A.encode country
      specCity = Spec.City {cityCode = specCityCode, cityName = Nothing}
      specCountry = Spec.Country {countryCode = specCountryCode, countryName = Nothing}
  Just $
    Spec.Location
      { locationAddress = Nothing,
        locationAreaCode = Nothing,
        locationCity = Just specCity,
        locationCountry = Just specCountry,
        locationGps = Nothing,
        locationId = Nothing,
        locationState = Nothing
      }

buildContextV2 :: (MonadFlow m) => Context.Action -> Context.Domain -> Text -> Maybe Text -> Text -> KP.BaseUrl -> Maybe Text -> Maybe KP.BaseUrl -> Context.City -> Context.Country -> m Spec.Context
buildContextV2 action domain messageId transactionId bapId bapUri bppId bppUri city country = do
  now <- getCurrentTime <&> Just
  pure $
    Spec.Context
      { contextAction = showContextAction action,
        contextBapId = Just bapId,
        contextBapUri = Just $ KP.showBaseUrl bapUri,
        contextBppId = bppId,
        contextBppUri = KP.showBaseUrl <$> bppUri,
        contextDomain = showContextDomain domain,
        contextKey = Nothing,
        contextLocation = buildContextLocation city country,
        contextMessageId = UUID.fromText messageId,
        contextTimestamp = now,
        contextTransactionId = UUID.fromText =<< transactionId,
        contextTtl = Nothing,
        contextVersion = Just "2.0.0"
      }

mkStops :: DSearchCommon.SearchReqLocation -> DSearchCommon.SearchReqLocation -> Maybe [Spec.Stop]
mkStops origin destination =
  let originGps = Gps.Gps {lat = origin.gps.lat, lon = origin.gps.lon}
      destinationGps = Gps.Gps {lat = destination.gps.lat, lon = destination.gps.lon}
   in Just
        [ Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = origin.address.building, -- JAYPAL, Confirm if it is correct to put it here
                      locationAreaCode = origin.address.areaCode,
                      locationCity = Just $ Spec.City Nothing origin.address.city,
                      locationCountry = Just $ Spec.Country Nothing origin.address.country,
                      locationGps = A.decode $ A.encode originGps,
                      locationState = Just $ Spec.State origin.address.state,
                      locationId = Nothing -- JAYPAL, Not sure what to keep here
                    },
              stopType = Just "START"
            },
          Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = origin.address.building, -- JAYPAL, Confirm if it is correct to put it here
                      locationAreaCode = destination.address.areaCode,
                      locationCity = Just $ Spec.City Nothing destination.address.city,
                      locationCountry = Just $ Spec.Country Nothing destination.address.country,
                      locationGps = A.decode $ A.encode destinationGps,
                      locationState = Just $ Spec.State destination.address.state,
                      locationId = Nothing -- JAYPAL, Not sure what to keep here
                    },
              stopType = Just "END"
            }
        ]
