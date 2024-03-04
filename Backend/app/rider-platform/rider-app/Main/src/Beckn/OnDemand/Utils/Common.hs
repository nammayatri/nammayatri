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

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import Control.Lens ((%~))
import qualified Data.Aeson as A
import qualified Data.List as List
import qualified Data.Text as T
import qualified Domain.Action.UI.Search as DSearch
import Domain.Types.BecknConfig
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.LocationAddress as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.Prelude hiding (id, state, (%~))
import Kernel.External.Maps as Maps
import qualified Kernel.Prelude as KP
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error

mkBapUri :: (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) => Id DM.Merchant -> m KP.BaseUrl
mkBapUri merchantId = asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchantId.getId)

mkStops :: DSearch.SearchReqLocation -> [DSearch.SearchReqLocation] -> UTCTime -> Maybe [Spec.Stop]
mkStops origin stops startTime =
  let originGps = Gps.Gps {lat = origin.gps.lat, lon = origin.gps.lon}
      destinationGps dest = Gps.Gps {lat = dest.gps.lat, lon = dest.gps.lon}
   in Just
        ( [ Spec.Stop
              { stopLocation =
                  Just $
                    Spec.Location
                      { locationAddress = Just $ mkAddress origin.address,
                        locationAreaCode = origin.address.areaCode,
                        locationCity = Just $ Spec.City Nothing origin.address.city,
                        locationCountry = Just $ Spec.Country Nothing origin.address.country,
                        locationGps = Utils.gpsToText originGps,
                        locationState = Just $ Spec.State origin.address.state,
                        locationUpdatedAt = Nothing,
                        locationId = Nothing
                      },
                stopType = Just $ show Enums.START,
                stopAuthorization = Nothing,
                stopTime = Just Spec.Time {timeTimestamp = Just startTime, timeDuration = Nothing}
              }
          ]
            <> ( ( \stop ->
                     Spec.Stop
                       { stopLocation =
                           Just $
                             Spec.Location
                               { locationAddress = Just $ mkAddress stop.address,
                                 locationAreaCode = stop.address.areaCode,
                                 locationCity = Just $ Spec.City Nothing stop.address.city,
                                 locationCountry = Just $ Spec.Country Nothing stop.address.country,
                                 locationGps = Utils.gpsToText $ destinationGps stop,
                                 locationState = Just $ Spec.State stop.address.state,
                                 locationId = Nothing,
                                 locationUpdatedAt = Nothing
                               },
                         stopType = Just $ show Enums.END,
                         stopAuthorization = Nothing,
                         stopTime = Nothing
                       }
                 )
                   <$> stops
               )
        )

mkAddress :: DLoc.LocationAddress -> Text
mkAddress DLoc.LocationAddress {..} =
  let res = map replaceEmpty [door, building, street, area, city, state, country]
   in T.intercalate ", " $ catMaybes res

replaceEmpty :: Maybe Text -> Maybe Text
replaceEmpty string = if string == Just "" then Nothing else string

mkPaymentTags :: Maybe [Spec.TagGroup]
mkPaymentTags =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.BUYER_FINDER_FEES,
                  descriptorName = Nothing,
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList = Just buyerFinderFeesSingleton
        },
      Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.SETTLEMENT_TERMS,
                  descriptorName = Nothing,
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              delayInterestSingleton
                ++ settlementTypeSingleton
                ++ staticTermsSingleton
        }
    ]
  where
    buyerFinderFeesSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tags.BUYER_FINDER_FEES_PERCENTAGE,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just "0"
          }
    delayInterestSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tags.DELAY_INTEREST,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just "0"
          }
    settlementTypeSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tags.SETTLEMENT_TYPE,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just "RSF"
          }
    staticTermsSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tags.STATIC_TERMS,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just "https://example-test-bap.com/static-terms.txt"
          }

castVehicleVariant :: VehVar.VehicleVariant -> (Text, Text)
castVehicleVariant = \case
  VehVar.SEDAN -> (show Enums.CAB, "SEDAN")
  VehVar.SUV -> (show Enums.CAB, "SUV")
  VehVar.HATCHBACK -> (show Enums.CAB, "HATCHBACK")
  VehVar.AUTO_RICKSHAW -> (show Enums.AUTO_RICKSHAW, "AUTO_RICKSHAW")
  VehVar.TAXI -> (show Enums.CAB, "TAXI")
  VehVar.TAXI_PLUS -> (show Enums.CAB, "TAXI_PLUS")

parseVehicleVariant :: Maybe Text -> Maybe Text -> Maybe VehVar.VehicleVariant
parseVehicleVariant mbCategory mbVariant =
  case (mbCategory, mbVariant) of
    (Just "CAB", Just "SEDAN") -> Just VehVar.SEDAN
    (Just "CAB", Just "SUV") -> Just VehVar.SUV
    (Just "CAB", Just "HATCHBACK") -> Just VehVar.HATCHBACK
    (Just "AUTO_RICKSHAW", Just "AUTO_RICKSHAW") -> Just VehVar.AUTO_RICKSHAW
    (Just "CAB", Just "TAXI") -> Just VehVar.TAXI
    (Just "CAB", Just "TAXI_PLUS") -> Just VehVar.TAXI_PLUS
    _ -> Nothing

castCancellationSourceV2 :: Text -> SBCR.CancellationSource
castCancellationSourceV2 = \case
  "ByUser" -> SBCR.ByUser
  "ByDriver" -> SBCR.ByDriver
  "ByMerchant" -> SBCR.ByMerchant
  "ByAllocator" -> SBCR.ByAllocator
  "ByApplication" -> SBCR.ByApplication
  _ -> SBCR.ByUser

getContextBppId :: MonadFlow m => Spec.Context -> m Text
getContextBppId context = do
  context.contextBppId & fromMaybeM (InvalidRequest "Missing contextBppId")

getMessageIdText :: MonadFlow m => Spec.Context -> m Text
getMessageIdText context = do
  messageUuid <- context.contextMessageId & fromMaybeM (InvalidRequest "Missing message_id")
  pure $ T.pack $ show messageUuid

parseGPS :: MonadFlow m => Text -> m Gps.Gps
parseGPS a =
  case T.splitOn "," a of
    [latStr, longStr] -> do
      lat :: Double <- readMaybe (T.unpack latStr) & fromMaybeM (InvalidRequest $ "Unable to parse GPS :" <> a)
      lon :: Double <- readMaybe (T.unpack longStr) & fromMaybeM (InvalidRequest $ "Unable to parse GPS :" <> a)
      return $ Gps.Gps lat lon
    _ -> throwError . InvalidRequest $ "Unable to parse GPS"

parseLatLong :: MonadFlow m => Text -> m Maps.LatLong
parseLatLong a =
  case T.splitOn "," a of
    [latStr, longStr] -> do
      lat :: Double <- readMaybe (T.unpack latStr) & fromMaybeM (InvalidRequest $ "Unable to parse GPS :" <> a)
      lon :: Double <- readMaybe (T.unpack longStr) & fromMaybeM (InvalidRequest $ "Unable to parse GPS :" <> a)
      return $ Maps.LatLong lat lon
    _ -> throwError . InvalidRequest $ "Unable to parse LatLong"

getContextBppUri :: MonadFlow m => Spec.Context -> m (Maybe BaseUrl)
getContextBppUri context = do
  let mbBppUriText = context.contextBppUri
  case mbBppUriText of
    Nothing -> pure Nothing
    Just bppUriText -> Just <$> A.decode (A.encode bppUriText) & fromMaybeM (InvalidRequest $ "Error in parsing contextBppUri: " <> bppUriText)

withTransactionIdLogTag :: (Log m) => Text -> m a -> m a
withTransactionIdLogTag = withTransactionIdLogTag'

mkStops' :: DLoc.Location -> Maybe DLoc.Location -> Maybe [Spec.Stop]
mkStops' origin mDestination =
  let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
   in Just $
        catMaybes
          [ Just $
              Spec.Stop
                { stopLocation =
                    Just $
                      Spec.Location
                        { locationAddress = Just $ mkAddress origin.address,
                          locationAreaCode = origin.address.areaCode,
                          locationCity = Just $ Spec.City Nothing origin.address.city,
                          locationCountry = Just $ Spec.Country Nothing origin.address.country,
                          locationGps = Utils.gpsToText originGps,
                          locationState = Just $ Spec.State origin.address.state,
                          locationId = Nothing,
                          locationUpdatedAt = Nothing
                        },
                  stopType = Just $ show Enums.START,
                  stopAuthorization = Nothing,
                  stopTime = Nothing
                },
            mDestination >>= \destination -> do
              let destinationGps = Gps.Gps {lat = destination.lat, lon = destination.lon}
              Just $
                Spec.Stop
                  { stopLocation =
                      Just $
                        Spec.Location
                          { locationAddress = Just $ mkAddress destination.address,
                            locationAreaCode = destination.address.areaCode,
                            locationCity = Just $ Spec.City Nothing destination.address.city,
                            locationCountry = Just $ Spec.Country Nothing destination.address.country,
                            locationGps = Utils.gpsToText destinationGps,
                            locationState = Just $ Spec.State destination.address.state,
                            locationId = Nothing,
                            locationUpdatedAt = Nothing
                          },
                    stopType = Just $ show Enums.END,
                    stopAuthorization = Nothing,
                    stopTime = Nothing
                  }
          ]

maskBillingNumber :: Text -> Text
maskBillingNumber billingNumber = do
  let startingDigitLen = 2
  let trailingDigitLen = 2
  let totalDigitLen = startingDigitLen + trailingDigitLen
  if T.length billingNumber <= totalDigitLen
    then billingNumber
    else
      T.take startingDigitLen billingNumber
        <> T.replicate (T.length billingNumber - totalDigitLen) "*"
        <> T.drop (T.length billingNumber - trailingDigitLen) billingNumber

mapVariantToVehicle :: VehVar.VehicleVariant -> VehicleCategory
mapVariantToVehicle variant = do
  case variant of
    VehVar.SEDAN -> CAB
    VehVar.HATCHBACK -> CAB
    VehVar.TAXI -> CAB
    VehVar.SUV -> CAB
    VehVar.TAXI_PLUS -> CAB
    VehVar.AUTO_RICKSHAW -> AUTO_RICKSHAW

getServiceTierName :: Spec.Item -> Maybe Text
getServiceTierName item = item.itemDescriptor >>= (.descriptorName)

mkRideTrackingRedisKey :: Text -> Text
mkRideTrackingRedisKey rideId = "RideTracking:" <> rideId
