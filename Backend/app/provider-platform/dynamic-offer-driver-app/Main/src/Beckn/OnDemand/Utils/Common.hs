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

import qualified Beckn.Types.Core.Taxi.OnSearch as OS
import qualified BecknV2.OnDemand.Types as Spec
import Control.Lens
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.Prelude hiding (id, state, view, (%~), (^?))
import Kernel.External.Maps as Maps
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common
import Kernel.Utils.Common
import Tools.Error

firstStop :: [Spec.Stop] -> Maybe Spec.Stop
firstStop = find (\stop -> Spec.stopType stop == Just "START")

lastStop :: [Spec.Stop] -> Maybe Spec.Stop
lastStop = find (\stop -> Spec.stopType stop == Just "END")

mkStops :: LatLong -> LatLong -> Maybe [Spec.Stop]
mkStops originGps destinationGps =
  Just
    [ Spec.Stop
        { stopLocation =
            Just $
              Spec.Location
                { locationAddress = Nothing, -- JAYPAL, Confirm if it is correct to put it here
                  locationAreaCode = Nothing,
                  locationCity = Nothing,
                  locationCountry = Nothing,
                  locationGps = A.decode $ A.encode originGps,
                  locationState = Nothing,
                  locationId = Nothing -- JAYPAL, Not sure what to keep here
                },
          stopType = Just "START",
          stopAuthorization = Nothing
        },
      Spec.Stop
        { stopLocation =
            Just $
              Spec.Location
                { locationAddress = Nothing, -- JAYPAL, Confirm if it is correct to put it here
                  locationAreaCode = Nothing,
                  locationCity = Nothing,
                  locationCountry = Nothing,
                  locationGps = A.decode $ A.encode destinationGps,
                  locationState = Nothing,
                  locationId = Nothing -- JAYPAL, Not sure what to keep here
                },
          stopType = Just "END",
          stopAuthorization = Nothing
        }
    ]

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

getContextCity :: MonadFlow m => Spec.Context -> m Context.City
getContextCity context = do
  location <- context.contextLocation & fromMaybeM (InvalidRequest "Missing contextLocation")
  city <- location.locationCity & fromMaybeM (InvalidRequest "Missing locationCity")
  cityText <- city.cityCode & fromMaybeM (InvalidRequest "Missing cityCode")
  decode (encode cityText) & fromMaybeM (InvalidRequest $ "Error in parsing cityCode: " <> cityText)

getContextCountry :: MonadFlow m => Spec.Context -> m Context.Country
getContextCountry context = do
  location <- context.contextLocation & fromMaybeM (InvalidRequest "Missing contextLocation")
  country <- location.locationCountry & fromMaybeM (InvalidRequest "Missing locationCountry")
  countryCodeText <- country.countryCode & fromMaybeM (InvalidRequest "Missing countryCode")
  decode (encode countryCodeText) & fromMaybeM (InvalidRequest $ "Error in parsing countryCode: " <> countryCodeText)

getContextBapUri :: MonadFlow m => Spec.Context -> m BaseUrl
getContextBapUri context = do
  bapUriText <- context.contextBapUri & fromMaybeM (InvalidRequest "Missing contextBapUri")
  decode (encode bapUriText) & fromMaybeM (InvalidRequest $ "Error in parsing contextBapUri: " <> bapUriText)

getContextBppUri :: MonadFlow m => Spec.Context -> m (Maybe BaseUrl)
getContextBppUri context = do
  let mbBppUriText = context.contextBppUri
  case mbBppUriText of
    Nothing -> pure Nothing
    Just bppUriText -> Just <$> decode (encodeUtf8 bppUriText) & fromMaybeM (InvalidRequest $ "Error in parsing contextBppUri: " <> bppUriText)

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

castVariant :: Variant.Variant -> Text
castVariant Variant.SEDAN = "SEDAN"
castVariant Variant.HATCHBACK = "HATCHBACK"
castVariant Variant.SUV = "SUV"
castVariant Variant.AUTO_RICKSHAW = "AUTO_RICKSHAW"
castVariant Variant.TAXI = "TAXI"
castVariant Variant.TAXI_PLUS = "TAXI_PLUS"

rationaliseMoney :: Money -> Text
rationaliseMoney fare = T.pack $ show $ OS.DecimalValue (toRational fare)

castDPaymentType :: DMPM.PaymentType -> Text
castDPaymentType DMPM.PREPAID = "ON_ORDER"
castDPaymentType DMPM.POSTPAID = "ON_FULFILLMENT"

parseVehicleVariant :: Text -> Maybe Variant.Variant
parseVehicleVariant = \case
  "SEDAN" -> Just Variant.SEDAN
  "SUV" -> Just Variant.SUV
  "HATCHBACK" -> Just Variant.HATCHBACK
  "AUTO_RICKSHAW" -> Just Variant.AUTO_RICKSHAW
  "TAXI" -> Just Variant.TAXI
  "TAXI_PLUS" -> Just Variant.TAXI_PLUS
  _ -> Nothing

parseAddress :: Spec.Location -> Maybe DL.LocationAddress
parseAddress Spec.Location {..} = do
  let areaCode = locationAreaCode
  let city = locationCity >>= (.cityName)
  let state = locationState >>= (.stateName)
  let country = locationCountry >>= (.countryName)
  let fullAddress = mkFullAddress city state country
  Just $
    DL.LocationAddress
      { street = Nothing,
        door = Nothing,
        building = Nothing,
        area = Nothing, -- TODO: Fetch this, discuss with ONDC
        ..
      }
  where
    mkFullAddress city state country = do
      let strictFields = catMaybes $ filter (not . isEmpty) [locationAddress, city, state, country]
      if null strictFields
        then Nothing
        else Just $ T.intercalate ", " strictFields

    isEmpty :: Maybe Text -> Bool
    isEmpty = maybe True (T.null . T.replace " " "")

mkStops' :: DLoc.Location -> DLoc.Location -> Maybe Text -> Maybe [Spec.Stop]
mkStops' origin destination mAuthorization =
  let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps = Gps.Gps {lat = destination.lat, lon = destination.lon}
   in Just
        [ Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = Just $ mkAddress origin.address,
                      locationAreaCode = origin.address.areaCode,
                      locationCity = Just $ Spec.City Nothing origin.address.city,
                      locationCountry = Just $ Spec.Country Nothing origin.address.country,
                      locationGps = A.decode $ A.encode originGps,
                      locationState = Just $ Spec.State origin.address.state,
                      locationId = Nothing
                    },
              stopType = Just "START",
              stopAuthorization = mAuthorization >>= mkAuthorization
            },
          Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = Just $ mkAddress destination.address,
                      locationAreaCode = destination.address.areaCode,
                      locationCity = Just $ Spec.City Nothing destination.address.city,
                      locationCountry = Just $ Spec.Country Nothing destination.address.country,
                      locationGps = A.decode $ A.encode destinationGps,
                      locationState = Just $ Spec.State destination.address.state,
                      locationId = Nothing
                    },
              stopType = Just "END",
              stopAuthorization = Nothing
            }
        ]
  where
    mkAddress :: DLoc.LocationAddress -> Text
    mkAddress DLoc.LocationAddress {..} = T.intercalate ", " $ catMaybes [door, building, street]

    mkAuthorization :: Text -> Maybe Spec.Authorization
    mkAuthorization auth =
      Just $
        Spec.Authorization
          { authorizationToken = Just auth,
            authorizationType = Just "OTP"
          }
