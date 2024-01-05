{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module BecknV2.OnDemand.Utils.Context
  ( buildContextV2,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import qualified Data.UUID as UUID
import EulerHS.Prelude hiding (id, (%~))
import qualified Kernel.Prelude as KP
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common

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
