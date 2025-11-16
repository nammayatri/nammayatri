{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.Init where

import qualified Beckn.OnDemand.Utils.Common as UCommon
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Payment as OUP
import Data.List (singleton)
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Location as Location
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.RiderConfig as DRC
import EulerHS.Prelude hiding (id, state)
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Utils.Common
import qualified SharedLogic.MerchantPaymentMethod as SLMPM

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
                        { locationAddress = Just $ UCommon.mkAddress origin.address,
                          locationAreaCode = origin.address.areaCode,
                          locationCity = Just $ Spec.City Nothing origin.address.city,
                          locationCountry = Just $ Spec.Country Nothing origin.address.country,
                          locationGps = Utils.gpsToText originGps,
                          locationState = Just $ Spec.State origin.address.state,
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
                  stopId = Just "0",
                  stopParentStopId = Nothing
                },
            ( \destination ->
                Spec.Stop
                  { stopLocation =
                      Just $
                        Spec.Location
                          { locationAddress = Just $ UCommon.mkAddress destination.address,
                            locationAreaCode = destination.address.areaCode,
                            locationCity = Just $ Spec.City Nothing destination.address.city,
                            locationCountry = Just $ Spec.Country Nothing destination.address.country,
                            locationGps = Utils.gpsToText (destinationGps destination),
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
            )
              <$> mDestination
          ]
          <> (map (\(location, order) -> UCommon.mkIntermediateStop location order (order - 1)) $ zip intermediateStops [1 ..])

mkPayment :: Maybe DMPM.PaymentMethodInfo -> DBC.BecknConfig -> DRC.RiderConfig -> Context.City -> [Spec.Payment]
mkPayment mbPaymentMethodInfo bapConfig riderConfig city = do
  let mkParams = SLMPM.mkBknPaymentParams mbPaymentMethodInfo bapConfig riderConfig
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
