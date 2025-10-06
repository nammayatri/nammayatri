{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.Search where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.Utils as Utils
import Control.Lens
import Data.Aeson
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Time
import qualified Domain.Types.RefereeLink as DRL
import EulerHS.Prelude hiding (id, view, (^?))
import Kernel.External.Maps as Maps
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Servant.API (FromHttpApiData (parseUrlPiece))
import Tools.Error (GenericError (InvalidRequest))

getPickUpTime :: Spec.SearchReqMessage -> Maybe Data.Time.UTCTime
getPickUpTime req =
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= firstStop
    >>= (.stopTime)
    >>= (.timeTimestamp)

getPickUpLocation :: MonadFlow m => Spec.SearchReqMessage -> m Spec.Location
getPickUpLocation req =
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= firstStop
    >>= (.stopLocation)
    & fromMaybeM (InvalidRequest "Missing Pickup Location")

getPickUp :: MonadFlow m => Spec.SearchReqMessage -> m Spec.Stop
getPickUp req =
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= firstStop
    & fromMaybeM (InvalidRequest "Missing Pickup")

getDropOffLocation :: Spec.SearchReqMessage -> Maybe Spec.Location
getDropOffLocation req = do
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= lastStop
    >>= (.stopLocation)

getIntermediateStops :: Spec.SearchReqMessage -> Maybe [Spec.Stop]
getIntermediateStops req = do
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= (Just . intermediateStops)

getIntermediateStopLocations :: MonadFlow m => Spec.SearchReqMessage -> m [Spec.Location]
getIntermediateStopLocations req = do
  pickup <- getPickUp req
  case getIntermediateStops req of
    Nothing -> return []
    Just stops -> do
      traverse getIntermediateStopLocation (sequenceStops pickup.stopId stops)
  where
    getIntermediateStopLocation :: MonadFlow m => Spec.Stop -> m Spec.Location
    getIntermediateStopLocation stop = stop.stopLocation & fromMaybeM (InvalidRequest ("Missing Stop Location" <> show stop))

getPickUpLocationGps :: MonadFlow m => Spec.SearchReqMessage -> m Text
getPickUpLocationGps req =
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= firstStop
    >>= (.stopLocation)
    >>= (.locationGps)
    & fromMaybeM (InvalidRequest "Missing Pickup Location GPS")

getDropOffLocationGps :: Spec.SearchReqMessage -> Maybe Text
getDropOffLocationGps req = do
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= lastStop
    >>= (.stopLocation)
    >>= (.locationGps)

getDistance :: Spec.SearchReqMessage -> Maybe Meters
getDistance req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  let tagValue = Utils.getTagV2 Tag.ROUTE_INFO Tag.DISTANCE_INFO_IN_M tagGroups
  Just . Meters =<< readMaybe . T.unpack =<< tagValue

getDuration :: Spec.SearchReqMessage -> Maybe Seconds
getDuration req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  let tagValue = Utils.getTagV2 Tag.ROUTE_INFO Tag.DURATION_INFO_IN_S tagGroups
  Just . Seconds =<< readMaybe . T.unpack =<< tagValue

getReturnTime :: Spec.SearchReqMessage -> Maybe Data.Time.UTCTime
getReturnTime req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  let tagValue = Utils.getTagV2 Tag.ROUTE_INFO Tag.RETURN_TIME tagGroups
  readMaybe . T.unpack =<< tagValue

getRoundTrip :: Spec.SearchReqMessage -> Maybe Bool
getRoundTrip req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  let tagValue = Utils.getTagV2 Tag.ROUTE_INFO Tag.ROUND_TRIP tagGroups
  readMaybe . T.unpack =<< tagValue

buildCustomerLanguage :: Spec.SearchReqMessage -> Maybe Language
buildCustomerLanguage req = customerLanguageFromCustomer <|> customerLanguageFromTag
  where
    customerLanguageFromCustomer =
      req.searchReqMessageIntent
        >>= (.intentFulfillment)
        >>= (.fulfillmentCustomer)
        >>= (.customerPerson)
        >>= (.personLanguages)
        >>= listToMaybe
        >>= (.languageCode)
        >>= (eitherToMaybe . parseUrlPiece)
    -- for backward compatibility
    customerLanguageFromTag = do
      let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentCustomer) >>= (.customerPerson) >>= (.personTags)
      let tagValue = Utils.getTagV2 Tag.CUSTOMER_INFO Tag.CUSTOMER_LANGUAGE tagGroups
      readMaybe . T.unpack =<< tagValue

buildCustomerNammaTags :: Spec.SearchReqMessage -> Maybe [LYT.TagNameValue]
buildCustomerNammaTags req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentCustomer) >>= (.customerPerson) >>= (.personTags)
  let tagValue = Utils.getTagV2 Tag.CUSTOMER_INFO Tag.CUSTOMER_NAMMA_TAGS tagGroups
  fmap LYT.TagNameValue <$> (readMaybe @[Text] . T.unpack =<< tagValue)

getIsMeterRideSearch :: Spec.SearchReqMessage -> Maybe Bool
getIsMeterRideSearch req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  let tagValue = Utils.getTagV2 Tag.SEARCH_REQUEST_INFO Tag.IS_METER_RIDE_SEARCH tagGroups
  readMaybe . T.unpack =<< tagValue

getIsMultimodalSearch :: Spec.SearchReqMessage -> Maybe Bool
getIsMultimodalSearch req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  let tagValue = Utils.getTagV2 Tag.SEARCH_REQUEST_INFO Tag.IS_MULTIMODAL_SEARCH tagGroups
  readMaybe . T.unpack =<< tagValue

checkIfDashboardSearch :: Spec.SearchReqMessage -> Maybe Bool
checkIfDashboardSearch req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentCustomer) >>= (.customerPerson) >>= (.personTags)
  let tagValue = Utils.getTagV2 Tag.CUSTOMER_INFO Tag.DASHBOARD_USER tagGroups
  readMaybe . T.unpack =<< tagValue

buildDisabilityTag :: Spec.SearchReqMessage -> Maybe Text
buildDisabilityTag req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentCustomer) >>= (.customerPerson) >>= (.personTags)
  Utils.getTagV2 Tag.CUSTOMER_INFO Tag.CUSTOMER_DISABILITY tagGroups

buildCustomerPhoneNumber :: Spec.SearchReqMessage -> Maybe Text
buildCustomerPhoneNumber req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentCustomer) >>= (.customerPerson) >>= (.personTags)
  Utils.getTagV2 Tag.CUSTOMER_INFO Tag.CUSTOMER_PHONE_NUMBER tagGroups

-- customerPerson <- req ^? (ix "searchReqMessageIntent" . key "intentFulfillment" . key "fulfillmentCustomer" . key "customerPerson" . key "tags") & fromMaybeM (InvalidRequest "Missing Fields")

getIsReallocationEnabled :: Spec.SearchReqMessage -> Maybe Bool
getIsReallocationEnabled req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
      tagValue = Utils.getTagV2 Tag.REALLOCATION_INFO Tag.IS_REALLOCATION_ENABLED tagGroups
  readMaybe . T.unpack =<< tagValue

fareParametersInRateCard :: Spec.SearchReqMessage -> Maybe Bool
fareParametersInRateCard req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
      tagValue = Utils.getTagV2 Tag.FARE_PARAMETERS_IN_RATECARD_INFO Tag.FARE_PARAMETERS_IN_RATECARD tagGroups
  readMaybe . T.unpack =<< tagValue

buildRoutePoints :: Spec.SearchReqMessage -> Maybe [Maps.LatLong]
buildRoutePoints req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  decode . encodeUtf8 =<< Utils.getTagV2 Tag.ROUTE_INFO Tag.WAYPOINTS tagGroups

buildMultipleRoutesTag :: Spec.SearchReqMessage -> Maybe [Maps.RouteInfo]
buildMultipleRoutesTag req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  decode . encodeUtf8 =<< Utils.getTagV2 Tag.ROUTE_INFO Tag.MULTIPLE_ROUTES tagGroups

getDriverIdentifier :: Spec.SearchReqMessage -> Maybe DRL.DriverIdentifier
getDriverIdentifier req = do
  let tagGroups = req.searchReqMessageIntent >>= (.intentFulfillment) >>= (.fulfillmentTags)
  decode . encodeUtf8 =<< Utils.getTagV2 Tag.DRIVER_IDENTIFIER Tag.DRIVER_IDENTITY tagGroups

firstStop :: [Spec.Stop] -> Maybe Spec.Stop
firstStop = find (\stop -> Spec.stopType stop == Just (show Enums.START))

lastStop :: [Spec.Stop] -> Maybe Spec.Stop
lastStop = find (\stop -> Spec.stopType stop == Just (show Enums.END))

intermediateStops :: [Spec.Stop] -> [Spec.Stop]
intermediateStops = filter (\stop -> Spec.stopType stop == Just (show Enums.INTERMEDIATE_STOP))

sequenceStops :: Maybe String -> [Spec.Stop] -> [Spec.Stop]
sequenceStops pickupStopId stops = go pickupStopId []
  where
    go mbPreviousStopId acc = do
      case mbPreviousStopId of
        Nothing -> acc
        Just previousStopId -> do
          case findNextStop previousStopId of
            Just nextStop -> go nextStop.stopId (acc ++ [nextStop])
            Nothing -> acc

    findNextStop :: String -> Maybe Spec.Stop
    findNextStop prevStopId = stops & find (\stop -> stop.stopParentStopId == Just prevStopId)
