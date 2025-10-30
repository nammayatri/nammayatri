{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.Init where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Payment as OUP
import Data.List (singleton)
import Domain.Types
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Location as Location
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import EulerHS.Prelude hiding (id, state)
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Utils.Common

mkStops :: Location.Location -> Maybe Location.Location -> Maybe Text -> [Location.Location] -> Maybe [Spec.Stop]
mkStops origin mDestination mStartOtp intermediateStops =
  let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps d = Gps.Gps {lat = d.lat, lon = d.lon}
   in Just $
        catMaybes
          [ Just $
              Spec.Stop
                { stopLocation =
                    Just $
                      Spec.Location
                        { locationAddress = Nothing, -- Not allowed in init request per ONDC spec
                          locationAreaCode = Nothing, -- Not allowed in init request per ONDC spec
                          locationCity = Nothing, -- Not allowed in init request per ONDC spec
                          locationCountry = Nothing, -- Not allowed in init request per ONDC spec
                          locationGps = Utils.gpsToText originGps,
                          locationState = Nothing, -- Not allowed in init request per ONDC spec
                          locationId = Nothing,
                          locationUpdatedAt = Nothing
                        },
                  stopType = Just $ show Enums.START,
                  stopAuthorization =
                    if isNothing mStartOtp
                      then Nothing
                      else
                        Just $
                          Spec.Authorization
                            { authorizationType = Just $ show Enums.OTP,
                              authorizationToken = mStartOtp
                            },
                  stopTime = Nothing,
                  stopId = Nothing, -- Not allowed in init request per ONDC spec
                  stopParentStopId = Nothing
                },
            ( \destination ->
                Spec.Stop
                  { stopLocation =
                      Just $
                        Spec.Location
                          { locationAddress = Nothing, -- Not allowed in init request per ONDC spec
                            locationAreaCode = Nothing, -- Not allowed in init request per ONDC spec
                            locationCity = Nothing, -- Not allowed in init request per ONDC spec
                            locationCountry = Nothing, -- Not allowed in init request per ONDC spec
                            locationGps = Utils.gpsToText (destinationGps destination),
                            locationState = Nothing, -- Not allowed in init request per ONDC spec
                            locationId = Nothing,
                            locationUpdatedAt = Nothing
                          },
                    stopType = Just $ show Enums.END,
                    stopAuthorization = Nothing,
                    stopTime = Nothing,
                    stopId = Nothing, -- Not allowed in init request per ONDC spec
                    stopParentStopId = Nothing -- Not allowed in init request per ONDC spec
                  }
            )
              <$> mDestination
          ]
          <> (map (\(location, order) -> mkIntermediateStopForInit location order (order - 1)) $ zip intermediateStops [1 ..])

mkIntermediateStopForInit :: Location.Location -> Int -> Int -> Spec.Stop
mkIntermediateStopForInit stop _stopId _parentStopId =
  let gps = Gps.Gps {lat = stop.lat, lon = stop.lon}
   in Spec.Stop
        { stopLocation =
            Just $
              Spec.Location
                { locationAddress = Nothing, -- Not allowed in init request per ONDC spec
                  locationAreaCode = Nothing, -- Not allowed in init request per ONDC spec
                  locationCity = Nothing, -- Not allowed in init request per ONDC spec
                  locationCountry = Nothing, -- Not allowed in init request per ONDC spec
                  locationGps = Utils.gpsToText gps,
                  locationState = Nothing, -- Not allowed in init request per ONDC spec
                  locationId = Just stop.id.getId,
                  locationUpdatedAt = Nothing
                },
          stopType = Just $ show Enums.INTERMEDIATE_STOP,
          stopAuthorization = Nothing,
          stopTime = Nothing,
          stopId = Nothing, -- Not allowed in init request per ONDC spec
          stopParentStopId = Nothing -- Not allowed in init request per ONDC spec
        }

mkPayment :: Maybe DMPM.PaymentMethodInfo -> DBC.BecknConfig -> Context.City -> [Spec.Payment]
mkPayment _ bapConfig city = do
  let mkParams :: (Maybe BknPaymentParams) = decodeFromText =<< bapConfig.paymentParamsJson
  singleton $ OUP.mkPayment (show city) (show bapConfig.collectedBy) Enums.NOT_PAID Nothing Nothing mkParams bapConfig.settlementType bapConfig.settlementWindow bapConfig.staticTermsUrl bapConfig.buyerFinderFee

castDPaymentType :: DMPM.PaymentType -> Text
castDPaymentType DMPM.ON_FULFILLMENT = show Enums.ON_FULFILLMENT
castDPaymentType DMPM.POSTPAID = show Enums.ON_FULFILLMENT

mkFulfillmentTags :: Maybe Distance -> Maybe [Spec.TagGroup]
mkFulfillmentTags mbMaxDistance = do
  if isJust mbMaxDistance
    then
      Just
        [ Spec.TagGroup
            { tagGroupDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tag.ESTIMATIONS,
                      descriptorName = Just $ show Tag.ESTIMATIONS,
                      descriptorShortDesc = Nothing
                    },
              tagGroupDisplay = Just True,
              tagGroupList =
                Just
                  [ Spec.Tag
                      { tagDescriptor =
                          Just $
                            Spec.Descriptor
                              { descriptorCode = (\_ -> Just $ show Tag.MAX_ESTIMATED_DISTANCE) =<< mbMaxDistance,
                                descriptorName = (\_ -> Just $ show Tag.MAX_ESTIMATED_DISTANCE) =<< mbMaxDistance,
                                descriptorShortDesc = Nothing
                              },
                        tagDisplay = (\_ -> Just True) =<< mbMaxDistance,
                        tagValue = (Just . show . distanceToHighPrecMeters) =<< mbMaxDistance
                      }
                  ]
            }
        ]
    else Nothing
