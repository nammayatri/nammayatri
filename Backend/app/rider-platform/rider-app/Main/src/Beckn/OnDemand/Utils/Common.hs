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
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.Utils as Utils
import Control.Lens ((%~))
import qualified Data.Aeson as A
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Time (TimeOfDay (..))
import qualified Domain.Action.Beckn.OnUpdate as OnUpdate
import qualified Domain.Action.UI.Search as DSearch
import Domain.Types.BecknConfig
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.EstimateRevised as DER
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.LocationAddress as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.Prelude hiding (id, state, (%~))
import Kernel.External.Maps as Maps
import Kernel.Prelude (roundToIntegral)
import qualified Kernel.Prelude as KP
import Kernel.Types.App
import Kernel.Types.Beckn.DecimalValue as DecimalValue
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
                        locationGps = A.decode $ A.encode originGps,
                        locationState = Just $ Spec.State origin.address.state,
                        locationId = Nothing
                      },
                stopType = Just $ show Enums.START,
                stopAuthorization = Nothing,
                stopTime = Just Spec.Time {timeTimestamp = Just startTime}
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
                                 locationGps = A.decode $ A.encode $ destinationGps stop,
                                 locationState = Just $ Spec.State stop.address.state,
                                 locationId = Nothing
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

parseLatLong :: MonadFlow m => Text -> m Maps.LatLong
parseLatLong a =
  case T.splitOn "," a of
    [latStr, longStr] ->
      let lat = fromMaybe 0.0 $ readMaybe $ T.unpack latStr
          lon = fromMaybe 0.0 $ readMaybe $ T.unpack longStr
       in return $ LatLong lat lon
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
                          locationGps = A.decode $ A.encode originGps,
                          locationState = Just $ Spec.State origin.address.state,
                          locationId = Nothing
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
                            locationGps = A.decode $ A.encode destinationGps,
                            locationState = Just $ Spec.State destination.address.state,
                            locationId = Nothing
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

buildEstimateBreakupList :: MonadFlow m => Spec.Item -> m [OnUpdate.EstimateBreakupInfo]
buildEstimateBreakupList item = do
  currency <-
    item.itemPrice
      >>= (.priceCurrency)
      & fromMaybeM (InvalidRequest "Missing Currency")
  tagGroups <- item.itemTags & fromMaybeM (InvalidRequest "Missing Tag Groups")
  tagGroupRateCard <- find (\tagGroup_ -> descriptorCode tagGroup_.tagGroupDescriptor == Just (show Tag.FARE_POLICY)) tagGroups & fromMaybeM (InvalidRequest "Missing fare policy") -- consume this from now on
  tagListRateCard <- tagGroupRateCard.tagGroupList & fromMaybeM (InvalidRequest "Missing Tag List")
  let breakups = map (buildEstimateBreakUpItem currency) tagListRateCard
  return (catMaybes breakups)
  where
    descriptorCode :: Maybe Spec.Descriptor -> Maybe Text
    descriptorCode (Just desc) = desc.descriptorCode
    descriptorCode Nothing = Nothing

buildEstimateBreakUpItem ::
  Text ->
  Spec.Tag ->
  Maybe OnUpdate.EstimateBreakupInfo
buildEstimateBreakUpItem currency tag = do
  descriptor <- tag.tagDescriptor
  value <- tag.tagValue
  tagValue <- DecimalValue.valueFromString value
  title <- descriptor.descriptorCode
  pure
    OnUpdate.EstimateBreakupInfo
      { title = title,
        price =
          OnUpdate.BreakupPriceInfo
            { currency = currency,
              value = Money $ roundToIntegral tagValue
            }
      }

getNightShiftCharge :: Maybe [Spec.TagGroup] -> Maybe Money
getNightShiftCharge tagGroup = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.NIGHT_CHARGE_MULTIPLIER tagGroup
  nightShiftCharge <- DecimalValue.valueFromString tagValue
  Just . Money $ roundToIntegral nightShiftCharge

getOldNightShiftCharge :: Maybe [Spec.TagGroup] -> Maybe DecimalValue
getOldNightShiftCharge tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.NIGHT_SHIFT_CHARGE tagGroups
  DecimalValue.valueFromString tagValue

getNightShiftStart :: Maybe [Spec.TagGroup] -> Maybe TimeOfDay
getNightShiftStart tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.NIGHT_SHIFT_START_TIME tagGroups
  readMaybe $ T.unpack tagValue

getNightShiftEnd :: Maybe [Spec.TagGroup] -> Maybe TimeOfDay
getNightShiftEnd tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.NIGHT_SHIFT_END_TIME tagGroups
  readMaybe $ T.unpack tagValue

getEstimatedFare :: MonadFlow m => Spec.Item -> m Money
getEstimatedFare item = do
  price <- item.itemPrice & fromMaybeM (InvalidRequest "Missing Price")
  let value = price.priceValue
  tagValue <- (DecimalValue.valueFromString =<< value) & fromMaybeM (InvalidRequest "Missing fare breakup item: tagValue")
  return $ Money $ roundToIntegral tagValue

getItemId :: MonadFlow m => Spec.Item -> m Text
getItemId item = do
  item.itemId & fromMaybeM (InvalidRequest "Missing Item Id")

buildWaitingChargeInfo' :: Maybe [Spec.TagGroup] -> Maybe Money
buildWaitingChargeInfo' tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.WAITING_CHARGE_PER_MIN tagGroups
  waitingChargeValue <- DecimalValue.valueFromString tagValue
  Just . Money $ roundToIntegral waitingChargeValue

buildWaitingChargeInfo :: MonadFlow m => Spec.Item -> m (Maybe OnUpdate.WaitingChargesInfo)
buildWaitingChargeInfo item = do
  let waitingChargePerMin' = buildWaitingChargeInfo' item.itemTags
  return $ ---------- FIX TODO___
    Just
      OnUpdate.WaitingChargesInfo
        { waitingChargePerMin = waitingChargePerMin'
        }

buildSpecialLocationTag :: MonadFlow m => Spec.Item -> m (Maybe Text)
buildSpecialLocationTag item =
  return $ Utils.getTagV2 Tag.INFO Tag.SPECIAL_LOCATION_TAG item.itemTags

getQuoteFulfillmentId :: MonadFlow m => Spec.Item -> m Text
getQuoteFulfillmentId item =
  item.itemFulfillmentIds
    >>= listToMaybe
    & fromMaybeM (InvalidRequest "Missing Fulfillment Ids")

getVehicleVariant :: MonadFlow m => Spec.Provider -> Spec.Item -> m VehVar.VehicleVariant
getVehicleVariant provider item = do
  let variant =
        item.itemFulfillmentIds >>= listToMaybe
          >>= (\fulfillmentId -> provider.providerFulfillments >>= find (\fulf -> fulf.fulfillmentId == Just fulfillmentId))
          >>= (.fulfillmentVehicle)
          >>= (.vehicleVariant)
      category =
        item.itemFulfillmentIds >>= listToMaybe
          >>= (\fulfillmentId -> provider.providerFulfillments >>= find (\fulf -> fulf.fulfillmentId == Just fulfillmentId))
          >>= (.fulfillmentVehicle)
          >>= (.vehicleCategory)
  case (category, variant) of
    (Just "CAB", Just "SEDAN") -> return VehVar.SEDAN
    (Just "CAB", Just "SUV") -> return VehVar.SUV
    (Just "CAB", Just "HATCHBACK") -> return VehVar.HATCHBACK
    (Just "AUTO_RICKSHAW", Just "AUTO_RICKSHAW") -> return VehVar.AUTO_RICKSHAW
    (Just "CAB", Just "TAXI") -> return VehVar.TAXI
    (Just "CAB", Just "TAXI_PLUS") -> return VehVar.TAXI_PLUS
    _ -> throwError (InvalidRequest $ "Unable to parse vehicle category:-" <> show category <> ",vehicle variant:-" <> show variant)

getProviderLocation :: MonadFlow m => Spec.Provider -> m [Maps.LatLong]
getProviderLocation provider = do
  locations <- provider.providerLocations & fromMaybeM (InvalidRequest "Missing Locations")
  mapM makeLatLong locations

buildNightShiftInfo :: Spec.Item -> Maybe OnUpdate.NightShiftInfo
buildNightShiftInfo item = do
  let itemTags = item.itemTags
  nightShiftCharge <- getNightShiftCharge itemTags
  let oldNightShiftCharge = getOldNightShiftCharge itemTags
  nightShiftStart <- getNightShiftStart itemTags
  nightShiftEnd <- getNightShiftEnd itemTags
  Just $
    OnUpdate.NightShiftInfo
      { oldNightShiftCharge = realToFrac <$> oldNightShiftCharge,
        ..
      }

getTotalFareRange :: MonadFlow m => Spec.Item -> m DER.FareRange
getTotalFareRange item = do
  minValue <-
    item.itemPrice
      >>= (.priceMinimumValue)
      >>= DecimalValue.valueFromString
      & fromMaybeM (InvalidRequest "Missing Minimum Value")
  maxValue <-
    item.itemPrice
      >>= (.priceMaximumValue)
      >>= DecimalValue.valueFromString
      & fromMaybeM (InvalidRequest "Missing Maximum Value")
  return $
    DER.FareRange
      { DER.minFare = Money $ roundToIntegral minValue,
        DER.maxFare = Money $ roundToIntegral maxValue
      }

makeLatLong :: MonadFlow m => Spec.Location -> m Maps.LatLong
makeLatLong location = do
  gps <- location.locationGps & fromMaybeM (InvalidRequest "Missing GPS")
  parseLatLong gps
