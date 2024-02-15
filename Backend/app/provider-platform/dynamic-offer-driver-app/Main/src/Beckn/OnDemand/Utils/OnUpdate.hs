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
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tags
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

-- TODO::Beckn, `Payment.ON_ORDER` Not present in spec.
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
        Fare.mkFareParamsBreakups (mkPrice currency) mkBreakupItem fareParams
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
      let title = breakup.quotationBreakupInnerTitle -- TODO::Beckn, all the titles are not present in spec.
      case fParamsType of
        DFParams.Progressive ->
          title
            `elem` [ Just (show Enums.BASE_FARE),
                     Just (show Enums.SERVICE_CHARGE),
                     Just (show Enums.DEAD_KILOMETER_FARE),
                     Just (show Enums.EXTRA_DISTANCE_FARE),
                     Just (show Enums.DRIVER_SELECTED_FARE),
                     Just (show Enums.CUSTOMER_SELECTED_FARE),
                     Just (show Enums.TOTAL_FARE),
                     Just (show Enums.WAITING_OR_PICKUP_CHARGES),
                     Just (show Enums.EXTRA_TIME_FARE),
                     Just (show Enums.CUSTOMER_CANCELLATION_DUES)
                   ]
        DFParams.Slab ->
          title
            `elem` [ Just (show Enums.BASE_FARE),
                     Just (show Enums.SERVICE_CHARGE),
                     Just (show Enums.WAITING_OR_PICKUP_CHARGES),
                     Just (show Enums.PLATFORM_FEE),
                     Just (show Enums.SGST),
                     Just (show Enums.CGST),
                     Just (show Enums.FIXED_GOVERNMENT_RATE),
                     Just (show Enums.TOTAL_FARE),
                     Just (show Enums.CUSTOMER_SELECTED_FARE),
                     Just (show Enums.NIGHT_SHIFT_CHARGE),
                     Just (show Enums.EXTRA_TIME_FARE),
                     Just (show Enums.CUSTOMER_CANCELLATION_DUES)
                   ]
        DFParams.Rental ->
          title
            `elem` [ Just (show Enums.BASE_FARE),
                     Just (show Enums.SERVICE_CHARGE),
                     Just (show Enums.DEAD_KILOMETER_FARE),
                     Just (show Enums.DIST_BASED_FARE),
                     Just (show Enums.TIME_BASED_FARE),
                     Just (show Enums.DRIVER_SELECTED_FARE),
                     Just (show Enums.CUSTOMER_SELECTED_FARE),
                     Just (show Enums.TOTAL_FARE),
                     Just (show Enums.WAITING_OR_PICKUP_CHARGES),
                     Just (show Enums.EXTRA_TIME_FARE),
                     Just (show Enums.CUSTOMER_CANCELLATION_DUES)
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
  let endOdometerReading :: Maybe Centesimal = (.value) <$> ride.endOdometerReading
  pure $
    Just
      [ Spec.TagGroup
          { tagGroupDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tags.RIDE_DISTANCE_DETAILS,
                    descriptorName = Just "Ride Distance Details",
                    descriptorShortDesc = Nothing
                  },
            tagGroupDisplay = Just False,
            tagGroupList =
              Just $
                chargeableDistanceSingleton chargeableDistance
                  ++ traveledDistanceSingleton traveledDistance
                  ++ endOdometerSingleton endOdometerReading
          }
      ]
  where
    chargeableDistanceSingleton chargeableDistance =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tags.CHARGEABLE_DISTANCE,
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
                  { descriptorCode = Just $ show Tags.TRAVELED_DISTANCE,
                    descriptorName = Just "Traveled Distance",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just $ show traveledDistance
          }

    endOdometerSingleton endOdometerReading =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tags.END_ODOMETER_READING,
                    descriptorName = Just "End Odometer Reading",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = show <$> endOdometerReading
          }

mkPreviousCancellationReasonsTags :: SBCR.CancellationSource -> Maybe [Spec.TagGroup]
mkPreviousCancellationReasonsTags cancellationSource =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.PREVIOUS_CANCELLATION_REASONS,
                  descriptorName = Just "Previous Cancellation Reasons",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just
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
                  { descriptorCode = Just $ show Tags.CANCELLATION_REASON,
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
                { descriptorCode = Just $ show Tags.DRIVER_NEW_MESSAGE,
                  descriptorName = Just "Driver New Message",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just
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
                  { descriptorCode = Just $ show Tags.MESSAGE,
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
                { descriptorCode = Just $ show Tags.SAFETY_ALERT,
                  descriptorName = Just "Safety Alert",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just
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
