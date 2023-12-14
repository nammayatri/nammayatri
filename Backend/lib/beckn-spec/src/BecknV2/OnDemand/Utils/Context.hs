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
    mapToCbAction,
    validateContext,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import qualified Data.UUID as UUID
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, (%~))
import qualified Kernel.Prelude as KP
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import qualified Kernel.Types.Error as Error
import Kernel.Utils.Common

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

mapToCbAction :: Text -> Maybe Text
mapToCbAction = \case
  "SEARCH" -> Just "ON_SEARCH"
  "SELECT" -> Just "ON_SELECT"
  "INIT" -> Just "ON_INIT"
  "CONFIRM" -> Just "ON_CONFIRM"
  "UPDATE" -> Just "ON_UPDATE"
  "STATUS" -> Just "ON_STATUS"
  "TRACK" -> Just "ON_TRACK"
  "CANCEL" -> Just "ON_CANCEL"
  "RATING" -> Just "ON_RATING"
  "SUPPORT" -> Just "ON_SUPPORT"
  _ -> Nothing

validateContext :: (HasFlowEnv m r '["_version" ::: Text]) => Context.Action -> Spec.Context -> m ()
validateContext action context = do
  validateDomain Context.MOBILITY context
  validateContextCommons action context

validateDomain :: (L.MonadFlow m, Log m) => Context.Domain -> Spec.Context -> m ()
validateDomain expectedDomain context = do
  domainText <- context.contextDomain & fromMaybeM (Error.InvalidRequest "Missing contextDomain")
  domain <- A.decode (A.encode domainText) & fromMaybeM (Error.InvalidRequest $ "Error in parsing contextDomain: " <> domainText <> " shrey00")
  unless (domain == expectedDomain) $
    throwError Error.InvalidDomain

validateContextCommons :: (HasFlowEnv m r '["_version" ::: Text], Log m) => Context.Action -> Spec.Context -> m ()
validateContextCommons expectedAction context = do
  validateAction expectedAction context
  validateCoreVersion context

validateAction :: (L.MonadFlow m, Log m) => Context.Action -> Spec.Context -> m ()
validateAction expectedAction context = do
  actionText <- context.contextAction & fromMaybeM (Error.InvalidRequest "Missing contextAction")
  action <- A.decode (A.encode actionText) & fromMaybeM (Error.InvalidRequest $ "Error in parsing contextAction: " <> actionText)
  -- convert context.contextAction to CoreContext.Action
  unless (action == expectedAction) $
    throwError Error.InvalidAction

validateCoreVersion :: (HasFlowEnv m r '["_version" ::: Text], Log m) => Spec.Context -> m ()
validateCoreVersion context = do
  supportedVersion <- asks (._version)
  version <- context.contextVersion & fromMaybeM (Error.InvalidRequest "Missing contextVersion")
  unless (version == supportedVersion) $
    throwError Error.UnsupportedCoreVer
