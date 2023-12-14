{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.Init where

import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import Data.List (singleton)
import qualified Domain.Types.Location as Location
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common (HighPrecMeters)

mkStops :: Location.Location -> Maybe Location.Location -> Maybe Text -> Maybe [Spec.Stop]
mkStops origin mDestination mStartOtp =
  let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps d = Gps.Gps {lat = d.lat, lon = d.lon}
   in Just
        [ Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = origin.address.building, -- JAYPAL, Confirm if it is correct to put it here
                      locationAreaCode = origin.address.areaCode,
                      locationCity = Just $ Spec.City Nothing origin.address.city,
                      locationCountry = Just $ Spec.Country Nothing origin.address.country,
                      locationGps = A.decode $ A.encode originGps,
                      locationState = Just $ Spec.State origin.address.state,
                      locationId = Nothing -- JAYPAL, Not sure what to keep here
                    },
              stopType = Just "START",
              stopAuthorization =
                if isNothing mStartOtp
                  then Nothing
                  else
                    Just $
                      Spec.Authorization
                        { authorizationType = Just "OTP",
                          authorizationToken = mStartOtp
                        }
            },
          Spec.Stop
            { stopLocation =
                mDestination
                  >>= \destination ->
                    Just $
                      Spec.Location
                        { locationAddress = destination.address.building, -- JAYPAL, Confirm if it is correct to put it here
                          locationAreaCode = destination.address.areaCode,
                          locationCity = Just $ Spec.City Nothing destination.address.city,
                          locationCountry = Just $ Spec.Country Nothing destination.address.country,
                          locationGps = A.decode $ A.encode (destinationGps destination),
                          locationState = Just $ Spec.State destination.address.state,
                          locationId = Nothing -- JAYPAL, Not sure what to keep here
                        },
              stopType = Just "END",
              stopAuthorization = Nothing
            }
        ]

mkPayment :: Maybe DMPM.PaymentMethodInfo -> [Spec.Payment]
mkPayment (Just DMPM.PaymentMethodInfo {..}) =
  singleton $
    Spec.Payment
      { paymentId = Nothing,
        paymentCollectedBy = Just "BPP",
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
        paymentStatus = Nothing,
        paymentTags = Nothing
      }
-- for backward compatibility
mkPayment Nothing =
  singleton
    Spec.Payment
      { paymentId = Nothing,
        paymentCollectedBy = Just "BPP",
        paymentType = Just "ON_FULFILLMENT",
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
castDPaymentType DMPM.PREPAID = "ON_ORDER"
castDPaymentType DMPM.POSTPAID = "ON_FULFILLMENT"

mkFulfillmentTags :: Maybe HighPrecMeters -> Maybe [Spec.TagGroup]
mkFulfillmentTags mbMaxDistance = do
  if isJust mbMaxDistance
    then
      Just $
        [ Spec.TagGroup
            { tagGroupDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "estimations",
                      descriptorName = Just "Estimations",
                      descriptorShortDesc = Nothing
                    },
              tagGroupDisplay = Just True,
              tagGroupList =
                Just $
                  [ Spec.Tag
                      { tagDescriptor =
                          Just $
                            Spec.Descriptor
                              { descriptorCode = (\_ -> Just "max_estimated_distance") =<< mbMaxDistance,
                                descriptorName = (\_ -> Just "Max Estimated Distance") =<< mbMaxDistance,
                                descriptorShortDesc = Nothing
                              },
                        tagDisplay = (\_ -> Just True) =<< mbMaxDistance,
                        tagValue = (Just . show) =<< mbMaxDistance
                      }
                  ]
            }
        ]
    else Nothing
