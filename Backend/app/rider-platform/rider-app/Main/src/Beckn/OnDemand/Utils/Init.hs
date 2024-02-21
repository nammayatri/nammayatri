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
import qualified Data.Aeson as A
import Data.List (singleton)
import qualified Domain.Types.Location as Location
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import EulerHS.Prelude hiding (id, state)
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common (HighPrecMeters)

mkStops :: Location.Location -> Maybe Location.Location -> Maybe Text -> Maybe [Spec.Stop]
mkStops origin mDestination mStartOtp =
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
                          locationGps = A.decode $ A.encode originGps,
                          locationState = Just $ Spec.State origin.address.state,
                          locationId = Nothing
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
                  stopTime = Nothing
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
                            locationGps = A.decode $ A.encode (destinationGps destination),
                            locationState = Just $ Spec.State destination.address.state,
                            locationId = Nothing
                          },
                    stopType = Just $ show Enums.END,
                    stopAuthorization = Nothing,
                    stopTime = Nothing
                  }
            )
              <$> mDestination
          ]

mkPayment :: Maybe DMPM.PaymentMethodInfo -> [Spec.Payment]
mkPayment (Just DMPM.PaymentMethodInfo {..}) =
  singleton $
    Spec.Payment
      { paymentId = Nothing,
        paymentCollectedBy = Just $ show Enums.BPP,
        paymentType = Just $ castDPaymentType paymentType,
        paymentParams =
          Just $
            Spec.PaymentParams
              { paymentParamsAmount = Nothing,
                paymentParamsBankAccountNumber = Nothing,
                paymentParamsBankCode = Nothing,
                paymentParamsCurrency = Just "INR",
                paymentParamsVirtualPaymentAddress = Nothing
              },
        paymentStatus = Just $ show Enums.NOT_PAID,
        paymentTags = Nothing
      }
-- for backward compatibility
mkPayment Nothing =
  singleton
    Spec.Payment
      { paymentId = Nothing,
        paymentCollectedBy = Just $ show Enums.BPP,
        paymentType = Just $ show Enums.ON_FULFILLMENT,
        paymentParams =
          Just $
            Spec.PaymentParams
              { paymentParamsAmount = Nothing,
                paymentParamsBankAccountNumber = Nothing,
                paymentParamsBankCode = Nothing,
                paymentParamsCurrency = Just "INR",
                paymentParamsVirtualPaymentAddress = Nothing
              },
        paymentStatus = Nothing,
        paymentTags = Nothing
      }

castDPaymentType :: DMPM.PaymentType -> Text
castDPaymentType DMPM.ON_FULFILLMENT = show Enums.ON_FULFILLMENT

mkFulfillmentTags :: Maybe HighPrecMeters -> Maybe [Spec.TagGroup]
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
                        tagValue = (Just . show) =<< mbMaxDistance
                      }
                  ]
            }
        ]
    else Nothing
