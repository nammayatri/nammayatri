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
import Data.Aeson.Text (encodeToTextBuilder)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Domain.Types.Merchant as DM
import EulerHS.Prelude hiding (id, (%~))
import qualified Kernel.Prelude as KP
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common

mkBapUri :: (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) => Id DM.Merchant -> m (Maybe Text)
mkBapUri merchantId = do
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchantId.getId)
  pure . Just $ KP.showBaseUrl bapUrl

contextSearchAction :: Maybe Text
contextSearchAction = Just . T.toLower $ show Context.SEARCH -- JAYPAL, Confirm if this is correct way

contextMobilityDomain :: Maybe Text
contextMobilityDomain = Just . LT.toStrict . TB.toLazyText . encodeToTextBuilder $ toJSON Context.MOBILITY

-- contextMobilityDomain = Just . T.toLower $ show Context.MOBILITY

buildContextLocation :: Context.City -> Context.Country -> Maybe Spec.Location
buildContextLocation city country = do
  let specCityCode = Just . LT.toStrict . TB.toLazyText . encodeToTextBuilder $ toJSON city
      specCountryCode = Just . LT.toStrict . TB.toLazyText . encodeToTextBuilder $ toJSON country
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
