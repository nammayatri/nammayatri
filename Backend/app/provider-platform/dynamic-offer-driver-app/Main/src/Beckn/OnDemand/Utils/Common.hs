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

import Beckn.ACL.Common (getTagV2)
import qualified BecknV2.OnDemand.Types as Spec
import Control.Lens
import Data.Aeson
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, view, (%~), (^?))
import Kernel.External.Maps as Maps
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import qualified Kernel.Types.Error as Error
import Kernel.Utils.Common
import Tools.Error

firstStop :: [Spec.Stop] -> Maybe Spec.Stop
firstStop = find (\stop -> Spec.stopType stop == Just "START")

lastStop :: [Spec.Stop] -> Maybe Spec.Stop
lastStop = find (\stop -> Spec.stopType stop == Just "END")

getPickUpLocation :: Spec.SearchReqMessage -> Spec.Location
getPickUpLocation req = do
  let intent = req.searchReqMessageIntent & fromMaybe (error "Missing Intent")
  let fulfillment = intent.intentFulfillment & fromMaybe (error "Missing Fulfillment")
  let stops = fulfillment.fulfillmentStops & fromMaybe (error "Missing Stops")
  let pickUp = firstStop stops & fromMaybe (error "Missing Pickup")
  pickUp.stopLocation & fromMaybe (error "Missing Location")

getDropOffLocation :: Spec.SearchReqMessage -> Spec.Location
getDropOffLocation req = do
  let intent = req.searchReqMessageIntent & fromMaybe (error "Missing Intent")
  let fulfillment = intent.intentFulfillment & fromMaybe (error "Missing Fulfillment")
  let stops = fulfillment.fulfillmentStops & fromMaybe (error "Missing Stops")
  let dropOff = lastStop stops & fromMaybe (error "Missing DropOff")
  dropOff.stopLocation & fromMaybe (error "Missing Location")

getPickUpLocationGps :: Spec.SearchReqMessage -> Text
getPickUpLocationGps req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let stops = fromMaybe (error "Missing Stops") $ fulfillment.fulfillmentStops
  let pickUp = lastStop stops & fromMaybe (error "Missing DropOff")
  let location = pickUp.stopLocation & fromMaybe (error "Missing Location")
  location.locationGps & fromMaybe (error "Missing GPS")

getDropOffLocationGps :: Spec.SearchReqMessage -> Text
getDropOffLocationGps req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let stops = fromMaybe (error "Missing Stops") $ fulfillment.fulfillmentStops
  let dropOff = lastStop stops & fromMaybe (error "Missing DropOff")
  let location = dropOff.stopLocation & fromMaybe (error "Missing Location")
  location.locationGps & fromMaybe (error "Missing GPS")

getDistance :: Spec.SearchReqMessage -> Maybe Meters
getDistance req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let tagGroups = fromMaybe (error "Missing Tags") $ fulfillment.fulfillmentTags
  tagValue <- getTagV2 "route_info" "distance_info_in_m" tagGroups
  distanceValue <- readMaybe $ T.unpack tagValue
  return $ Meters distanceValue

getDuration :: Spec.SearchReqMessage -> Maybe Seconds
getDuration req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let tagGroups = fromMaybe (error "Missing Tags") $ fulfillment.fulfillmentTags
  tagValue <- getTagV2 "route_info" "duration_info_in_s" tagGroups
  durationValue <- readMaybe $ T.unpack tagValue
  Just $ Seconds durationValue

buildCustomerLanguage :: Spec.SearchReqMessage -> Maybe Language
buildCustomerLanguage req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let customer = fromMaybe (error "Missing Customer") $ fulfillment.fulfillmentCustomer
  let customerPerson = fromMaybe (error "Missing Person") $ customer.customerPerson
  let tagGroups = fromMaybe (error "Missing Tags") $ customerPerson.personTags
  tagValue <- getTagV2 "customer_info" "customer_language" tagGroups
  readMaybe $ T.unpack tagValue

buildDisabilityTag :: Spec.SearchReqMessage -> Maybe Text
buildDisabilityTag req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let customer = fromMaybe (error "Missing Customer") $ fulfillment.fulfillmentCustomer
  let customerPerson = fromMaybe (error "Missing Person") $ customer.customerPerson
  let tagGroups = fromMaybe (error "Missing Tags") $ customerPerson.personTags
  tagValue <- getTagV2 "customer_info" "disability_tag" tagGroups
  Just tagValue

buildCustomerPhoneNumber :: Spec.SearchReqMessage -> Maybe Text
buildCustomerPhoneNumber req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let customer = fromMaybe (error "Missing Customer") $ fulfillment.fulfillmentCustomer
  let customerPerson = fromMaybe (error "Missing Person") $ customer.customerPerson
  let tagGroups = fromMaybe (error "Missing Tags") $ customerPerson.personTags
  tagValue <- getTagV2 "customer_info" "customer_phone_number" tagGroups
  readMaybe $ T.unpack tagValue

-- customerPerson <- req ^? (ix "searchReqMessageIntent" . key "intentFulfillment" . key "fulfillmentCustomer" . key "customerPerson" . key "tags") & fromMaybeM (InvalidRequest "Missing Fields")

getIsReallocationEnabled :: Spec.SearchReqMessage -> Maybe Bool
getIsReallocationEnabled req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let tagGroups = fromMaybe (error "Missing Tags") $ fulfillment.fulfillmentTags
  tagValue <- getTagV2 "reallocation_info" "is_reallocation_enabled" tagGroups
  readMaybe $ T.unpack tagValue

buildRoutePoints :: Spec.SearchReqMessage -> Maybe [Maps.LatLong]
buildRoutePoints req = do
  let intent = fromMaybe (error "Missing Intent") $ req.searchReqMessageIntent
  let fulfillment = fromMaybe (error "Missing Fulfillment") $ intent.intentFulfillment
  let tagGroups = fromMaybe (error "Missing Tags") $ fulfillment.fulfillmentTags
  getTagV2 "route_info" "route_points" tagGroups >>= decode . encodeUtf8

parseLatLong :: Text -> LatLong
parseLatLong a =
  case T.splitOn "," a of
    [latStr, longStr] ->
      let lat = fromMaybe 0.0 $ readMaybe $ T.unpack latStr
          lon = fromMaybe 0.0 $ readMaybe $ T.unpack longStr
       in LatLong lat lon
    _ -> error "Unable to parse LatLong"

getTransactionId :: MonadFlow m => Spec.Context -> m Text
getTransactionId context = do
  transactionUuid <- context.contextTransactionId & fromMaybeM (InvalidRequest "Missing transaction_id")
  pure $ T.pack $ show transactionUuid

getMessageId :: MonadFlow m => Spec.Context -> m Text
getMessageId context = do
  messageUuid <- context.contextMessageId & fromMaybeM (InvalidRequest "Missing message_id")
  pure $ T.pack $ show messageUuid

validateContext :: (HasFlowEnv m r '["_version" ::: Text]) => Context.Action -> Spec.Context -> m ()
validateContext action context = do
  validateDomain Context.MOBILITY context
  validateContextCommons action context

validateDomain :: (L.MonadFlow m, Log m) => Context.Domain -> Spec.Context -> m ()
validateDomain expectedDomain context = do
  domainText <- context.contextDomain & fromMaybeM (InvalidRequest "Missing contextDomain")
  domain <- (decode $ encode domainText) & fromMaybeM (InvalidRequest $ "Error in parsing contextDomain: " <> domainText <> " shrey00")
  unless (domain == expectedDomain) $
    throwError Error.InvalidDomain

validateContextCommons :: (HasFlowEnv m r '["_version" ::: Text], Log m) => Context.Action -> Spec.Context -> m ()
validateContextCommons expectedAction context = do
  validateAction expectedAction context
  validateCoreVersion context

validateAction :: (L.MonadFlow m, Log m) => Context.Action -> Spec.Context -> m ()
validateAction expectedAction context = do
  actionText <- context.contextAction & fromMaybeM (InvalidRequest "Missing contextAction")
  action <- (decode $ encode actionText) & fromMaybeM (InvalidRequest $ "Error in parsing contextAction: " <> actionText)
  -- convert context.contextAction to CoreContext.Action
  unless (action == expectedAction) $
    throwError Error.InvalidAction

validateCoreVersion :: (HasFlowEnv m r '["_version" ::: Text], Log m) => Spec.Context -> m ()
validateCoreVersion context = do
  supportedVersion <- asks (._version)
  version <- context.contextVersion & fromMaybeM (InvalidRequest "Missing contextVersion")
  unless (version == supportedVersion) $
    throwError Error.UnsupportedCoreVer

getContextCity :: MonadFlow m => Spec.Context -> m Context.City
getContextCity context = do
  location <- context.contextLocation & fromMaybeM (InvalidRequest "Missing contextLocation")
  city <- location.locationCity & fromMaybeM (InvalidRequest "Missing locationCity")
  cityText <- city.cityCode & fromMaybeM (InvalidRequest "Missing cityCode")
  (decode $ encode cityText) & fromMaybeM (InvalidRequest $ "Error in parsing cityCode: " <> cityText)

getContextCountry :: MonadFlow m => Spec.Context -> m Context.Country
getContextCountry context = do
  location <- context.contextLocation & fromMaybeM (InvalidRequest "Missing contextLocation")
  country <- location.locationCountry & fromMaybeM (InvalidRequest "Missing locationCountry")
  countryCodeText <- country.countryCode & fromMaybeM (InvalidRequest "Missing countryCode")
  (decode $ encode countryCodeText) & fromMaybeM (InvalidRequest $ "Error in parsing countryCode: " <> countryCodeText)

getContextBapUri :: MonadFlow m => Spec.Context -> m BaseUrl
getContextBapUri context = do
  bapUriText <- context.contextBapUri & fromMaybeM (InvalidRequest "Missing contextBapUri")
  (decode $ encode bapUriText) & fromMaybeM (InvalidRequest $ "Error in parsing contextBapUri: " <> bapUriText)

getContextBppUri :: MonadFlow m => Spec.Context -> m (Maybe BaseUrl)
getContextBppUri context = do
  let mbBppUriText = context.contextBppUri
  case mbBppUriText of
    Nothing -> pure Nothing
    Just bppUriText -> Just <$> (decode $ encodeUtf8 bppUriText) & fromMaybeM (InvalidRequest $ "Error in parsing contextBppUri: " <> bppUriText)

withTransactionIdLogTag :: (Log m) => Text -> m a -> m a
withTransactionIdLogTag = withTransactionIdLogTag'

getContextBapId :: MonadFlow m => Spec.Context -> m Text
getContextBapId context = do
  context.contextBapId & fromMaybeM (InvalidRequest "Missing contextBapId")

mkBppUri ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Text ->
  m BaseUrl
mkBppUri merchantId =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack merchantId)
