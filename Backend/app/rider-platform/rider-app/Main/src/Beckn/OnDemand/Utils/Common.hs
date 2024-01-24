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
import qualified Data.Aeson as A
import qualified Data.List as List
import qualified Data.Text as T
import qualified Domain.Action.UI.Search as DSearch
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.LocationAddress as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as SearchRequest
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

mkStops :: DSearch.SearchReqLocation -> [DSearch.SearchReqLocation] -> Maybe [Spec.Stop]
mkStops origin stops =
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
                stopType = Just "START",
                stopAuthorization = Nothing,
                stopTime = Nothing
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
                         stopType = Just "END",
                         stopAuthorization = Nothing,
                         stopTime = Nothing
                       }
                 )
                   <$> stops
               )
        )
  where
    mkAddress :: DLoc.LocationAddress -> Text
    mkAddress DLoc.LocationAddress {..} = T.intercalate ", " $ catMaybes [door, building, street]

mkPaymentTags :: Maybe [Spec.TagGroup]
mkPaymentTags =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "BUYER_FINDER_FEES",
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
                { descriptorCode = Just "SETTLEMENT_TERMS",
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
                  { descriptorCode = Just "BUYER_FINDER_FEES_PERCENTAGE",
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
                  { descriptorCode = Just "DELAY_INTEREST",
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
                  { descriptorCode = Just "SETTLEMENT_TYPE",
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
                  { descriptorCode = Just "STATIC_TERMS",
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just "https://example-test-bap.com/static-terms.txt"
          }

castVehicleVariant :: VehVar.VehicleVariant -> (Text, Text)
castVehicleVariant = \case
  VehVar.SEDAN -> ("CAB", "SEDAN")
  VehVar.SUV -> ("CAB", "SUV")
  VehVar.HATCHBACK -> ("CAB", "HATCHBACK")
  VehVar.AUTO_RICKSHAW -> ("AUTO_RICKSHAW", "AUTO_RICKSHAW")
  VehVar.TAXI -> ("CAB", "TAXI")
  VehVar.TAXI_PLUS -> ("CAB", "TAXI_PLUS")

castCancellationSourceV2 :: Text -> SBCR.CancellationSource
castCancellationSourceV2 = \case
  "ByUser" -> SBCR.ByUser
  "ByDriver" -> SBCR.ByDriver
  "ByMerchant" -> SBCR.ByMerchant
  "ByAllocator" -> SBCR.ByAllocator
  "ByApplication" -> SBCR.ByApplication
  _ -> SBCR.ByUser

getContextBapId :: MonadFlow m => Spec.Context -> m Text
getContextBapId context = do
  context.contextBapId & fromMaybeM (InvalidRequest "Missing contextBapId")

getMessageId :: MonadFlow m => Spec.Context -> m (Id SearchRequest.SearchRequest)
getMessageId context = do
  messageUuid <- context.contextMessageId & fromMaybeM (InvalidRequest "Missing message_id")
  pure $ Id $ T.pack $ show messageUuid

getMessageIdText :: MonadFlow m => Spec.Context -> m Text
getMessageIdText context = do
  messageUuid <- context.contextMessageId & fromMaybeM (InvalidRequest "Missing message_id")
  pure $ T.pack $ show messageUuid

parseLatLong :: Text -> Maps.LatLong
parseLatLong a =
  case T.splitOn "," a of
    [latStr, longStr] ->
      let lat = fromMaybe 0.0 $ readMaybe $ T.unpack latStr
          lon = fromMaybe 0.0 $ readMaybe $ T.unpack longStr
       in LatLong lat lon
    _ -> error "Unable to parse LatLong"

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
                  stopType = Just "START",
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
                    stopType = Just "END",
                    stopAuthorization = Nothing,
                    stopTime = Nothing
                  }
          ]
  where
    mkAddress :: DLoc.LocationAddress -> Text
    mkAddress DLoc.LocationAddress {..} = T.intercalate ", " $ catMaybes [door, building, street]
