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
import Domain.Types.BecknConfig
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.LocationAddress as DLoc
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as MOC
import qualified Domain.Types.ServiceTierType as DVST
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
                stopId = Just "0",
                stopParentStopId = Nothing,
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
                         stopTime = Nothing,
                         stopId = Just $ show (length intermediateStops + 1),
                         stopParentStopId = Just $ show (length intermediateStops)
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
                      stopTime = Nothing,
                      stopId = Just "0",
                      stopParentStopId = Nothing
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
                      stopTime = Nothing,
                      stopId = Just $ show (length intermediateStops + 1),
                      stopParentStopId = Just $ show (length intermediateStops)
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
   in Spec.Stop
        { stopLocation =
            Just $
              Spec.Location
                { locationAddress = Just $ mkAddress stop.address,
                  locationAreaCode = stop.address.areaCode,
                  locationCity = Just $ Spec.City Nothing stop.address.city,
                  locationCountry = Just $ Spec.Country Nothing stop.address.country,
                  locationGps = Utils.gpsToText gps,
                  locationState = Just $ Spec.State stop.address.state,
                  locationId = Just stop.id.getId,
                  locationUpdatedAt = Nothing
                },
          stopType = Just $ show Enums.INTERMEDIATE_STOP,
          stopAuthorization = Nothing,
          stopTime = Nothing,
          stopId = Just "0",
          stopParentStopId = Nothing ---------used only for add stop in rental, no need to handle sequence.--------
        }

mkIntermediateStop :: DLoc.Location -> Int -> Int -> Spec.Stop
mkIntermediateStop stop id parentStopId =
  let gps = Gps.Gps {lat = stop.lat, lon = stop.lon}
   in Spec.Stop
        { stopLocation =
            Just $
              Spec.Location
                { locationAddress = Just $ mkAddress stop.address,
                  locationAreaCode = stop.address.areaCode,
                  locationCity = Just $ Spec.City Nothing stop.address.city,
                  locationCountry = Just $ Spec.Country Nothing stop.address.country,
                  locationGps = Utils.gpsToText gps,
                  locationState = Just $ Spec.State stop.address.state,
                  locationId = Just stop.id.getId,
                  locationUpdatedAt = Nothing
                },
          stopType = Just $ show Enums.INTERMEDIATE_STOP,
          stopAuthorization = Nothing,
          stopTime = Nothing,
          stopId = Just $ show id,
          stopParentStopId = Just $ show parentStopId
        }

mkIntermediateStopSearch :: SLS.SearchReqLocation -> Int -> Int -> Spec.Stop
mkIntermediateStopSearch stop id parentStopId =
  let gps = Gps.Gps {lat = stop.gps.lat, lon = stop.gps.lon}
   in Spec.Stop
        { stopLocation =
            Just $
              Spec.Location
                { locationAddress = Just $ mkAddress stop.address,
                  locationAreaCode = stop.address.areaCode,
                  locationCity = Just $ Spec.City Nothing stop.address.city,
                  locationCountry = Just $ Spec.Country Nothing stop.address.country,
                  locationGps = Utils.gpsToText gps,
                  locationState = Just $ Spec.State stop.address.state,
                  locationId = Nothing,
                  locationUpdatedAt = Nothing
                },
          stopType = Just $ show Enums.INTERMEDIATE_STOP,
          stopAuthorization = Nothing,
          stopTime = Nothing,
          stopId = Just $ show id,
          stopParentStopId = Just $ show parentStopId
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
