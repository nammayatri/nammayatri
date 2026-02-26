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
import BecknV2.OnDemand.Utils.Constructors
import qualified BecknV2.OnDemand.Utils.Common as Utils
import Control.Lens ((%~))
import qualified Data.Aeson as A
import qualified Data.Text as T
import Domain.Types.BecknConfig
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.LocationAddress as DLoc
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as MOC
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.Prelude hiding (id, state, (%~))
import qualified Kernel.External.Maps as Maps
import qualified Kernel.Prelude as KP
import Kernel.Types.App
import Kernel.Types.Beckn.DecimalValue as DecimalValue
import Kernel.Types.Beckn.Domain as Domain
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Search as SLS
import qualified Storage.CachedQueries.BlackListOrg as QBlackList
import qualified Storage.CachedQueries.VehicleConfig as CQVC
import qualified Storage.CachedQueries.WhiteListOrg as QWhiteList
import Tools.Error

mkBapUri :: (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) => Id DM.Merchant -> m KP.BaseUrl
mkBapUri merchantId = asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchantId.getId)

mkStops :: SLS.SearchReqLocation -> [SLS.SearchReqLocation] -> UTCTime -> Maybe [Spec.Stop]
mkStops origin stops startTime =
  let originGps = Gps.Gps {lat = origin.gps.lat, lon = origin.gps.lon}
      destinationGps dest = Gps.Gps {lat = dest.gps.lat, lon = dest.gps.lon}
      destination = [KP.last stops | not (null stops)]
      intermediateStops = KP.safeInit stops
   in Just
        ( [ emptyStop
              { Spec.stopLocation =
                  Just $
                    emptyLocation
                      { Spec.locationAddress = Just $ mkAddress origin.address,
                        Spec.locationAreaCode = origin.address.areaCode,
                        Spec.locationCity = Just $ Spec.City Nothing origin.address.city,
                        Spec.locationCountry = Just $ Spec.Country Nothing origin.address.country,
                        Spec.locationGps = Utils.gpsToText originGps,
                        Spec.locationState = Just $ Spec.State origin.address.state
                      },
                Spec.stopType = Just $ show Enums.START,
                Spec.stopId = Just "0",
                Spec.stopTime = Just Spec.Time {timeTimestamp = Just startTime, timeDuration = Nothing}
              }
          ]
            <> ( ( \stop ->
                     emptyStop
                       { Spec.stopLocation =
                           Just $
                             emptyLocation
                               { Spec.locationAddress = Just $ mkAddress stop.address,
                                 Spec.locationAreaCode = stop.address.areaCode,
                                 Spec.locationCity = Just $ Spec.City Nothing stop.address.city,
                                 Spec.locationCountry = Just $ Spec.Country Nothing stop.address.country,
                                 Spec.locationGps = Utils.gpsToText $ destinationGps stop,
                                 Spec.locationState = Just $ Spec.State stop.address.state
                               },
                         Spec.stopType = Just $ show Enums.END,
                         Spec.stopId = Just $ show (length intermediateStops + 1),
                         Spec.stopParentStopId = Just $ show (length intermediateStops)
                       }
                 )
                   <$> destination
               )
            <> (map (\(location, order) -> mkIntermediateStopSearch location order (order - 1)) $ zip intermediateStops [1 ..])
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
    [ Tags.getFullTagGroup Tags.BUYER_FINDER_FEES
        [ Tags.mkTag Tags.BUYER_FINDER_FEES_PERCENTAGE (Just "0")
        ],
      Tags.getFullTagGroup Tags.SETTLEMENT_TERMS
        [ Tags.mkTag Tags.DELAY_INTEREST (Just "0"),
          Tags.mkTag Tags.SETTLEMENT_TYPE (Just "RSF"),
          Tags.mkTag Tags.STATIC_TERMS (Just "https://example-test-bap.com/static-terms.txt")
        ]
    ]

castVehicleVariant :: VehVar.VehicleVariant -> (Text, Text)
castVehicleVariant = \case
  VehVar.SEDAN -> (show Enums.CAB, "SEDAN")
  VehVar.SUV -> (show Enums.CAB, "SUV")
  VehVar.HATCHBACK -> (show Enums.CAB, "HATCHBACK")
  VehVar.AUTO_RICKSHAW -> (show Enums.AUTO_RICKSHAW, "AUTO_RICKSHAW")
  VehVar.TAXI -> (show Enums.CAB, "TAXI")
  VehVar.TAXI_PLUS -> (show Enums.CAB, "TAXI_PLUS")
  VehVar.PREMIUM_SEDAN -> (show Enums.CAB, "PREMIUM_SEDAN")
  VehVar.BLACK -> (show Enums.CAB, "BLACK")
  VehVar.BLACK_XL -> (show Enums.CAB, "BLACK_XL")
  VehVar.BIKE -> (show Enums.TWO_WHEELER, "BIKE") -- When parsing from beckn to domain, convert to MOTORCYCLE
  VehVar.DELIVERY_BIKE -> (show Enums.TWO_WHEELER, "DELIVERY_BIKE")
  VehVar.AMBULANCE_TAXI -> (show Enums.AMBULANCE, "AMBULANCE_TAXI")
  VehVar.AMBULANCE_TAXI_OXY -> (show Enums.AMBULANCE, "AMBULANCE_TAXI_OXY")
  VehVar.AMBULANCE_AC -> (show Enums.AMBULANCE, "AMBULANCE_AC")
  VehVar.AMBULANCE_AC_OXY -> (show Enums.AMBULANCE, "AMBULANCE_AC_OXY")
  VehVar.AMBULANCE_VENTILATOR -> (show Enums.AMBULANCE, "AMBULANCE_VENTILATOR")
  VehVar.SUV_PLUS -> (show Enums.CAB, "SUV_PLUS")
  VehVar.HERITAGE_CAB -> (show Enums.CAB, "HERITAGE_CAB")
  VehVar.EV_AUTO_RICKSHAW -> (show Enums.AUTO_RICKSHAW, "EV_AUTO_RICKSHAW")
  VehVar.DELIVERY_LIGHT_GOODS_VEHICLE -> (show Enums.TRUCK, "DELIVERY_LIGHT_GOODS_VEHICLE")
  VehVar.DELIVERY_TRUCK_MINI -> (show Enums.TRUCK, "DELIVERY_TRUCK_MINI")
  VehVar.DELIVERY_TRUCK_SMALL -> (show Enums.TRUCK, "DELIVERY_TRUCK_SMALL")
  VehVar.DELIVERY_TRUCK_MEDIUM -> (show Enums.TRUCK, "DELIVERY_TRUCK_MEDIUM")
  VehVar.DELIVERY_TRUCK_LARGE -> (show Enums.TRUCK, "DELIVERY_TRUCK_LARGE")
  VehVar.DELIVERY_TRUCK_ULTRA_LARGE -> (show Enums.TRUCK, "DELIVERY_TRUCK_ULTRA_LARGE")
  VehVar.BUS_NON_AC -> (show Enums.BUS, "BUS_NON_AC")
  VehVar.BUS_AC -> (show Enums.BUS, "BUS_AC")
  VehVar.AUTO_PLUS -> (show Enums.AUTO_RICKSHAW, "AUTO_PLUS")
  VehVar.BOAT -> (show Enums.BOAT, "BOAT")
  VehVar.VIP_ESCORT -> (show Enums.CAB, "VIP_ESCORT")
  VehVar.VIP_OFFICER -> (show Enums.CAB, "VIP_OFFICER")
  VehVar.AC_PRIORITY -> (show Enums.CAB, "AC_PRIORITY")
  VehVar.BIKE_PLUS -> (show Enums.TWO_WHEELER, "BIKE_PLUS")
  VehVar.E_RICKSHAW -> (show Enums.AUTO_RICKSHAW, "E_RICKSHAW")
  VehVar.AUTO_LITE -> (show Enums.AUTO_RICKSHAW, "AUTO_LITE")
  VehVar.PINK_AUTO -> (show Enums.AUTO_RICKSHAW, "PINK_AUTO")

parseVehicleVariant :: Maybe Text -> Maybe Text -> Maybe VehVar.VehicleVariant
parseVehicleVariant mbCategory mbVariant =
  case (mbCategory, mbVariant) of
    (Just "CAB", Just "SEDAN") -> Just VehVar.SEDAN
    (Just "CAB", Just "SUV") -> Just VehVar.SUV
    (Just "CAB", Just "HATCHBACK") -> Just VehVar.HATCHBACK
    (Just "AUTO_RICKSHAW", Just "AUTO_RICKSHAW") -> Just VehVar.AUTO_RICKSHAW
    (Just "CAB", Just "TAXI") -> Just VehVar.TAXI
    (Just "CAB", Just "TAXI_PLUS") -> Just VehVar.TAXI_PLUS
    (Just "CAB", Just "BLACK") -> Just VehVar.BLACK
    (Just "CAB", Just "BLACK_XL") -> Just VehVar.BLACK_XL
    (Just "CAB", Just "PREMIUM_SEDAN") -> Just VehVar.PREMIUM_SEDAN
    (Just "MOTORCYCLE", Just "BIKE") -> Just VehVar.BIKE -- becomes redundant, TODO : remove in next release
    (Just "TWO_WHEELER", Just "BIKE") -> Just VehVar.BIKE
    (Just "TWO_WHEELER", Just "DELIVERY_BIKE") -> Just VehVar.DELIVERY_BIKE
    (Just "AMBULANCE", Just "AMBULANCE_TAXI") -> Just VehVar.AMBULANCE_TAXI
    (Just "AMBULANCE", Just "AMBULANCE_TAXI_OXY") -> Just VehVar.AMBULANCE_TAXI_OXY
    (Just "AMBULANCE", Just "AMBULANCE_AC") -> Just VehVar.AMBULANCE_AC
    (Just "AMBULANCE", Just "AMBULANCE_AC_OXY") -> Just VehVar.AMBULANCE_AC_OXY
    (Just "AMBULANCE", Just "AMBULANCE_VENTILATOR") -> Just VehVar.AMBULANCE_VENTILATOR
    (Just "CAB", Just "SUV_PLUS") -> Just VehVar.SUV_PLUS
    (Just "CAB", Just "HERITAGE_CAB") -> Just VehVar.HERITAGE_CAB
    (Just "AUTO_RICKSHAW", Just "EV_AUTO_RICKSHAW") -> Just VehVar.EV_AUTO_RICKSHAW
    (Just "AUTO_RICKSHAW", Just "AUTO_PLUS") -> Just VehVar.AUTO_PLUS
    (Just "TRUCK", Just "DELIVERY_LIGHT_GOODS_VEHICLE") -> Just VehVar.DELIVERY_LIGHT_GOODS_VEHICLE
    (Just "TRUCK", Just "DELIVERY_TRUCK_MINI") -> Just VehVar.DELIVERY_TRUCK_MINI
    (Just "TRUCK", Just "DELIVERY_TRUCK_SMALL") -> Just VehVar.DELIVERY_TRUCK_SMALL
    (Just "TRUCK", Just "DELIVERY_TRUCK_MEDIUM") -> Just VehVar.DELIVERY_TRUCK_MEDIUM
    (Just "TRUCK", Just "DELIVERY_TRUCK_LARGE") -> Just VehVar.DELIVERY_TRUCK_LARGE
    (Just "TRUCK", Just "DELIVERY_TRUCK_ULTRA_LARGE") -> Just VehVar.DELIVERY_TRUCK_ULTRA_LARGE
    (Just "BOAT", Just "BOAT") -> Just VehVar.BOAT
    (Just "CAB", Just "VIP_ESCORT") -> Just VehVar.VIP_ESCORT
    (Just "CAB", Just "VIP_OFFICER") -> Just VehVar.VIP_OFFICER
    (Just "CAB", Just "AC_PRIORITY") -> Just VehVar.AC_PRIORITY
    (Just "TWO_WHEELER", Just "BIKE_PLUS") -> Just VehVar.BIKE_PLUS
    (Just "MOTORCYCLE", Just "BIKE_PLUS") -> Just VehVar.BIKE_PLUS
    (Just "AUTO_RICKSHAW", Just "E_RICKSHAW") -> Just VehVar.E_RICKSHAW
    (Just "AUTO_RICKSHAW", Just "AUTO_LITE") -> Just VehVar.AUTO_LITE
    (Just "AUTO_RICKSHAW", Just "PINK_AUTO") -> Just VehVar.PINK_AUTO
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

mkStops' :: Maybe DLoc.Location -> [DLoc.Location] -> Maybe DLoc.Location -> Maybe [Spec.Stop]
mkStops' mbOrigin intermediateStops mDestination = do
  let stops =
         ( catMaybes
            [ mbOrigin >>= \origin -> do
                let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
                Just $
                  emptyStop
                    { Spec.stopLocation =
                        Just $
                          emptyLocation
                            { Spec.locationAddress = Just $ mkAddress origin.address,
                              Spec.locationAreaCode = origin.address.areaCode,
                              Spec.locationCity = Just $ Spec.City Nothing origin.address.city,
                              Spec.locationCountry = Just $ Spec.Country Nothing origin.address.country,
                              Spec.locationGps = Utils.gpsToText originGps,
                              Spec.locationState = Just $ Spec.State origin.address.state
                            },
                      Spec.stopType = Just $ show Enums.START,
                      Spec.stopId = Just "0"
                    },
              mDestination >>= \destination -> do
                let destinationGps = Gps.Gps {lat = destination.lat, lon = destination.lon}
                Just $
                  emptyStop
                    { Spec.stopLocation =
                        Just $
                          emptyLocation
                            { Spec.locationAddress = Just $ mkAddress destination.address,
                              Spec.locationAreaCode = destination.address.areaCode,
                              Spec.locationCity = Just $ Spec.City Nothing destination.address.city,
                              Spec.locationCountry = Just $ Spec.Country Nothing destination.address.country,
                              Spec.locationGps = Utils.gpsToText destinationGps,
                              Spec.locationState = Just $ Spec.State destination.address.state
                            },
                      Spec.stopType = Just $ show Enums.END,
                      Spec.stopId = Just $ show (length intermediateStops + 1),
                      Spec.stopParentStopId = Just $ show (length intermediateStops)
                    }
            ]
        )
          <> (map (\(location, order) -> mkIntermediateStop location order (order - 1)) $ zip intermediateStops [1 ..])
  if length stops == 0
    then Nothing
    else Just stops

makeStop :: DLoc.Location -> Spec.Stop
makeStop stop =
  let gps = Gps.Gps {lat = stop.lat, lon = stop.lon}
   in emptyStop
        { Spec.stopLocation =
            Just $
              emptyLocation
                { Spec.locationAddress = Just $ mkAddress stop.address,
                  Spec.locationAreaCode = stop.address.areaCode,
                  Spec.locationCity = Just $ Spec.City Nothing stop.address.city,
                  Spec.locationCountry = Just $ Spec.Country Nothing stop.address.country,
                  Spec.locationGps = Utils.gpsToText gps,
                  Spec.locationState = Just $ Spec.State stop.address.state,
                  Spec.locationId = Just stop.id.getId
                },
          Spec.stopType = Just $ show Enums.INTERMEDIATE_STOP,
          Spec.stopId = Just "0"
        }

mkIntermediateStop :: DLoc.Location -> Int -> Int -> Spec.Stop
mkIntermediateStop stop id parentStopId =
  let gps = Gps.Gps {lat = stop.lat, lon = stop.lon}
   in emptyStop
        { Spec.stopLocation =
            Just $
              emptyLocation
                { Spec.locationAddress = Just $ mkAddress stop.address,
                  Spec.locationAreaCode = stop.address.areaCode,
                  Spec.locationCity = Just $ Spec.City Nothing stop.address.city,
                  Spec.locationCountry = Just $ Spec.Country Nothing stop.address.country,
                  Spec.locationGps = Utils.gpsToText gps,
                  Spec.locationState = Just $ Spec.State stop.address.state,
                  Spec.locationId = Just stop.id.getId
                },
          Spec.stopType = Just $ show Enums.INTERMEDIATE_STOP,
          Spec.stopId = Just $ show id,
          Spec.stopParentStopId = Just $ show parentStopId
        }

mkIntermediateStopSearch :: SLS.SearchReqLocation -> Int -> Int -> Spec.Stop
mkIntermediateStopSearch stop id parentStopId =
  let gps = Gps.Gps {lat = stop.gps.lat, lon = stop.gps.lon}
   in emptyStop
        { Spec.stopLocation =
            Just $
              emptyLocation
                { Spec.locationAddress = Just $ mkAddress stop.address,
                  Spec.locationAreaCode = stop.address.areaCode,
                  Spec.locationCity = Just $ Spec.City Nothing stop.address.city,
                  Spec.locationCountry = Just $ Spec.Country Nothing stop.address.country,
                  Spec.locationGps = Utils.gpsToText gps,
                  Spec.locationState = Just $ Spec.State stop.address.state
                },
          Spec.stopType = Just $ show Enums.INTERMEDIATE_STOP,
          Spec.stopId = Just $ show id,
          Spec.stopParentStopId = Just $ show parentStopId
        }

mapTextToVehicle :: Text -> Maybe Enums.VehicleCategory
mapTextToVehicle = \case
  "AUTO_RICKSHAW" -> Just Enums.AUTO_RICKSHAW
  "AUTO_PLUS" -> Just Enums.AUTO_RICKSHAW
  "CAB" -> Just Enums.CAB
  "TWO_WHEELER" -> Just Enums.MOTORCYCLE
  "MOTORCYCLE" -> Just Enums.MOTORCYCLE
  "AMBULANCE" -> Just Enums.AMBULANCE
  "TRUCK" -> Just Enums.TRUCK
  "AUTO_LITE" -> Just Enums.AUTO_RICKSHAW
  "PINK_AUTO" -> Just Enums.AUTO_RICKSHAW
  "BUS" -> Just Enums.BUS
  _ -> Nothing

getServiceTierType :: Spec.Item -> Maybe DVST.ServiceTierType
getServiceTierType item = item.itemDescriptor >>= (.descriptorCode) >>= (readMaybe . T.unpack)

getServiceTierName :: Spec.Item -> Maybe Text
getServiceTierName item = item.itemDescriptor >>= (.descriptorName)

getServiceTierShortDesc :: Spec.Item -> Maybe Text
getServiceTierShortDesc item = item.itemDescriptor >>= (.descriptorShortDesc)

mkRideTrackingRedisKey :: Text -> Text
mkRideTrackingRedisKey rideId = "RideTracking:" <> rideId

decimalValueToPrice :: Currency -> DecimalValue.DecimalValue -> Price
decimalValueToPrice currency (DecimalValue.DecimalValue v) = do
  Price
    { amountInt = Money $ KP.roundToIntegral v,
      amount = HighPrecMoney v,
      currency
    }

validateSubscriber :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Id DM.Merchant -> Id MOC.MerchantOperatingCity -> m ()
validateSubscriber subscriberId merchantId merchantOperatingCityId = do
  totalSubIds <- QWhiteList.countTotalSubscribers
  void $
    if totalSubIds == 0
      then do
        checkBlacklisted subscriberId
      else do
        checkWhitelisted merchantId merchantOperatingCityId subscriberId

checkBlacklisted :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m ()
checkBlacklisted subscriberId = do
  whenM (isBlackListed subscriberId Domain.MOBILITY) . throwError . InvalidRequest $
    "It is a Blacklisted subscriber " <> subscriberId

isBlackListed :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Domain -> m Bool
isBlackListed subscriberId domain = QBlackList.findBySubscriberIdAndDomain (ShortId subscriberId) domain <&> isJust

checkWhitelisted :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Id MOC.MerchantOperatingCity -> Text -> m ()
checkWhitelisted merchantId merchantOperatingCityId subscriberId = do
  whenM (isNotWhiteListed subscriberId Domain.MOBILITY merchantId merchantOperatingCityId) . throwError . InvalidRequest $
    "It is not a whitelisted subscriber " <> subscriberId

isNotWhiteListed :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Domain -> Id DM.Merchant -> Id MOC.MerchantOperatingCity -> m Bool
isNotWhiteListed subscriberId domain merchantId merchantOperatingCityId = QWhiteList.findBySubscriberIdDomainMerchantIdAndMerchantOperatingCityId (ShortId subscriberId) domain merchantId merchantOperatingCityId <&> isNothing

getBlackListedVehicles :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id BecknConfig -> Text -> m [Enums.VehicleCategory]
getBlackListedVehicles becknConfigId subscriberId = do
  vehicleConfigs <- CQVC.findAllByBecknConfigId becknConfigId
  let blackListedVehicles = filter (\vc -> subscriberId `elem` vc.blackListedSubscribers) vehicleConfigs
  pure $ mapMaybe (\blv -> mapTextToVehicle blv.category) blackListedVehicles
