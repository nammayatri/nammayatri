{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.OnUpdate where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingCancelledEvent as BookingCancelledOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as OnUpdate
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.List as List
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id, (%~))
import Kernel.Types.Common
import Kernel.Utils.Common
import SharedLogic.FareCalculator as Fare
import Tools.Error

mkRideCompletedPaymentType :: Maybe DMPM.PaymentMethodInfo -> Text
mkRideCompletedPaymentType = show . maybe OnUpdate.ON_FULFILLMENT (Common.castDPaymentType . (.paymentType))

showPaymentCollectedBy :: Maybe DMPM.PaymentMethodInfo -> Text
showPaymentCollectedBy = show . maybe OnUpdate.BPP (Common.castDPaymentCollector . (.collectedBy))

mkRideCompletedQuote :: MonadFlow m => DRide.Ride -> DFParams.FareParameters -> m Spec.Quotation
mkRideCompletedQuote ride fareParams = do
  fare' <- ride.fare & fromMaybeM (InternalError "Ride fare is not present in RideCompletedReq ride.")
  let fare = Utils.rationaliseMoney fare'
  let currency = "INR"
      price =
        Spec.Price
          { priceComputedValue = Just fare,
            priceCurrency = Just currency,
            priceValue = Just fare,
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Nothing
          }
      breakup =
        Fare.mkBreakupList (mkPrice currency) mkBreakupItem fareParams
          & filter (filterRequiredBreakups $ DFParams.getFareParametersType fareParams)
  pure
    Spec.Quotation
      { quotationBreakup = Just breakup,
        quotationPrice = Just price,
        quotationTtl = Nothing
      }
  where
    mkPrice currency val =
      Spec.Price
        { priceCurrency = Just currency,
          priceValue = Just $ Utils.rationaliseMoney val,
          priceComputedValue = Nothing,
          priceMaximumValue = Nothing,
          priceMinimumValue = Nothing,
          priceOfferedValue = Nothing
        }

    mkBreakupItem :: Text -> Spec.Price -> Spec.QuotationBreakupInner
    mkBreakupItem title price =
      Spec.QuotationBreakupInner
        { quotationBreakupInnerTitle = Just title,
          quotationBreakupInnerPrice = Just price
        }

    filterRequiredBreakups fParamsType breakup = do
      let title = breakup.quotationBreakupInnerTitle
      case fParamsType of
        DFParams.Progressive ->
          title
            `elem` [ Just "BASE_FARE",
                     Just "SERVICE_CHARGE",
                     Just "DEAD_KILOMETER_FARE",
                     Just "EXTRA_DISTANCE_FARE",
                     Just "DRIVER_SELECTED_FARE",
                     Just "CUSTOMER_SELECTED_FARE",
                     Just "TOTAL_FARE",
                     Just "WAITING_OR_PICKUP_CHARGES",
                     Just "EXTRA_TIME_FARE",
                     Just "CUSTOMER_CANCELLATION_DUES"
                   ]
        DFParams.Slab ->
          title
            `elem` [ Just "BASE_FARE",
                     Just "SERVICE_CHARGE",
                     Just "WAITING_OR_PICKUP_CHARGES",
                     Just "PLATFORM_FEE",
                     Just "SGST",
                     Just "CGST",
                     Just "FIXED_GOVERNMENT_RATE",
                     Just "TOTAL_FARE",
                     Just "CUSTOMER_SELECTED_FARE",
                     Just "NIGHT_SHIFT_CHARGE",
                     Just "EXTRA_TIME_FARE",
                     Just "CUSTOMER_CANCELLATION_DUES"
                   ]

mkRideCompletedPayment :: Maybe DMPM.PaymentMethodInfo -> Maybe Text -> [Spec.Payment]
mkRideCompletedPayment paymentMethodInfo _paymentUrl = do
  let currency = "INR"
  List.singleton $
    Spec.Payment
      { paymentCollectedBy = Just $ showPaymentCollectedBy paymentMethodInfo,
        paymentParams =
          Just $
            Spec.PaymentParams
              { paymentParamsCurrency = Just currency,
                paymentParamsAmount = Nothing,
                paymentParamsBankAccountNumber = Nothing,
                paymentParamsBankCode = Nothing,
                paymentParamsVirtualPaymentAddress = Nothing
              },
        paymentType = Just $ mkRideCompletedPaymentType paymentMethodInfo,
        paymentId = Nothing,
        paymentStatus = Nothing,
        paymentTags = Nothing
      }

mkDistanceTagGroup :: MonadFlow m => DRide.Ride -> m (Maybe [Spec.TagGroup])
mkDistanceTagGroup ride = do
  chargeableDistance :: HighPrecMeters <-
    realToFrac <$> ride.chargeableDistance
      & fromMaybeM (InternalError "Ride chargeable distance is not present in OnUpdateBuildReq ride.")
  let traveledDistance :: HighPrecMeters = ride.traveledDistance
  pure $
    Just
      [ Spec.TagGroup
          { tagGroupDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "ride_distance_details",
                    descriptorName = Just "Ride Distance Details",
                    descriptorShortDesc = Nothing
                  },
            tagGroupDisplay = Just False,
            tagGroupList =
              Just $
                chargeableDistanceSingleton chargeableDistance
                  ++ traveledDistanceSingleton traveledDistance
          }
      ]
  where
    chargeableDistanceSingleton chargeableDistance =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "chargeable_distance",
                    descriptorName = Just "Chargeable Distance",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just $ show chargeableDistance
          }

    traveledDistanceSingleton traveledDistance =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "traveled_distance",
                    descriptorName = Just "Traveled Distance",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just $ show traveledDistance
          }

mkDriverArrivedInfoTags :: Maybe UTCTime -> Maybe [Spec.TagGroup]
mkDriverArrivedInfoTags arrivalTime =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "driver_arrived_info",
                  descriptorName = Just "Driver Arrived Info",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              arrivalTimeSingleton
        }
    ]
  where
    arrivalTimeSingleton
      | isNothing arrivalTime = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "arrival_time",
                      descriptorName = Just "Chargeable Distance",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = show <$> arrivalTime
            }

mkPreviousCancellationReasonsTags :: SBCR.CancellationSource -> Maybe [Spec.TagGroup]
mkPreviousCancellationReasonsTags cancellationSource =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "previous_cancellation_reasons",
                  descriptorName = Just "Previous Cancellation Reasons",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              cancellationSourceSingleton
        }
    ]
  where
    cancellationSourceSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "cancellation_reason",
                    descriptorName = Just "Chargeable Distance",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just . show $ castCancellationSource cancellationSource
          }

castCancellationSource :: SBCR.CancellationSource -> BookingCancelledOU.CancellationSource
castCancellationSource = \case
  SBCR.ByUser -> BookingCancelledOU.ByUser
  SBCR.ByDriver -> BookingCancelledOU.ByDriver
  SBCR.ByMerchant -> BookingCancelledOU.ByMerchant
  SBCR.ByAllocator -> BookingCancelledOU.ByAllocator
  SBCR.ByApplication -> BookingCancelledOU.ByApplication

mkNewMessageTags :: Text -> Maybe [Spec.TagGroup]
mkNewMessageTags message =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "driver_new_message",
                  descriptorName = Just "Driver New Message",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              messageSingleton
        }
    ]
  where
    messageSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "message",
                    descriptorName = Just "New Message",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just message
          }

mkSafetyAlertTags :: Text -> Text -> Maybe [Spec.TagGroup]
mkSafetyAlertTags reason code =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "safety_alert",
                  descriptorName = Just "Safety Alert",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              safetyAlertTriggerSingleton
        }
    ]
  where
    safetyAlertTriggerSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just code,
                    descriptorName = Just "Safety Alert Trigger",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just reason
          }
