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
import BecknV2.OnDemand.Utils.Payment
import qualified Data.List as List
import qualified Data.Text as T
import Domain.Types
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.FarePolicy as FarePolicyD
import Domain.Types.Merchant
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id, (%~))
import Kernel.Types.Common hiding (mkPrice)
import qualified Kernel.Types.Common as Common
import Kernel.Types.Id
import Kernel.Utils.Common hiding (mkPrice)
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
  let fare = highPrecMoneyToText fare'
  let currency = show ride.currency
      price =
        Spec.Price
          { priceCurrency = Just currency,
            priceValue = Just fare,
            priceComputedValue = Nothing,
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Nothing
          }
      breakup =
        Fare.mkFareParamsBreakups (mkPrice' currency) mkBreakupItem fareParams
          & filter (filterRequiredBreakups $ DFParams.getFareParametersType fareParams)
  pure
    Spec.Quotation
      { quotationBreakup = Just breakup,
        quotationPrice = Just price,
        quotationTtl = Nothing
      }
  where
    mkPrice' currency val =
      Spec.Price
        { priceCurrency = Just currency,
          priceValue = Just $ highPrecMoneyToText val,
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
                     Just (show Enums.DISTANCE_FARE),
                     Just (show Enums.DRIVER_SELECTED_FARE),
                     Just (show Enums.CUSTOMER_SELECTED_FARE),
                     Just (show Enums.TOTAL_FARE),
                     Just (show Enums.CONGESTION_CHARGE),
                     Just (show Enums.WAITING_OR_PICKUP_CHARGES),
                     Just (show Enums.EXTRA_TIME_FARE),
                     Just (show Enums.CANCELLATION_CHARGES),
                     Just (show Enums.PET_CHARGES),
                     Just (show Enums.BUSINESS_DISCOUNT),
                     Just (show Enums.PERSONAL_DISCOUNT),
                     Just (show Enums.TOLL_CHARGES),
                     Just (show Enums.PARKING_CHARGE),
                     Just (show Enums.RIDE_STOP_CHARGES),
                     Just (show Enums.PER_STOP_CHARGES),
                     Just (show Enums.NIGHT_SHIFT_CHARGE),
                     Just (show Enums.LUGGAGE_CHARGE),
                     Just (show Enums.RETURN_FEE),
                     Just (show Enums.BOOTH_CHARGE)
                   ]
        DFParams.Slab ->
          title
            `elem` [ Just (show Enums.BASE_FARE),
                     Just (show Enums.SERVICE_CHARGE),
                     Just (show Enums.WAITING_OR_PICKUP_CHARGES),
                     Just (show Enums.PLATFORM_FEE),
                     Just (show Enums.SGST),
                     Just (show Enums.CGST),
                     Just (show Enums.CONGESTION_CHARGE),
                     Just (show Enums.FIXED_GOVERNMENT_RATE),
                     Just (show Enums.TOTAL_FARE),
                     Just (show Enums.CUSTOMER_SELECTED_FARE),
                     Just (show Enums.NIGHT_SHIFT_CHARGE),
                     Just (show Enums.BUSINESS_DISCOUNT),
                     Just (show Enums.PERSONAL_DISCOUNT),
                     Just (show Enums.EXTRA_TIME_FARE),
                     Just (show Enums.CANCELLATION_CHARGES),
                     Just (show Enums.TOLL_CHARGES),
                     Just (show Enums.PET_CHARGES),
                     Just (show Enums.PARKING_CHARGE),
                     Just (show Enums.LUGGAGE_CHARGE),
                     Just (show Enums.RETURN_FEE),
                     Just (show Enums.BOOTH_CHARGE)
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
                     Just (show Enums.NIGHT_SHIFT_CHARGE),
                     Just (show Enums.EXTRA_TIME_FARE),
                     Just (show Enums.BUSINESS_DISCOUNT),
                     Just (show Enums.PERSONAL_DISCOUNT),
                     Just (show Enums.CANCELLATION_CHARGES),
                     Just (show Enums.PET_CHARGES),
                     Just (show Enums.PARKING_CHARGE),
                     Just (show Enums.LUGGAGE_CHARGE),
                     Just (show Enums.RETURN_FEE),
                     Just (show Enums.BOOTH_CHARGE)
                   ]
        DFParams.InterCity ->
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
                     Just (show Enums.NIGHT_SHIFT_CHARGE),
                     Just (show Enums.EXTRA_TIME_FARE),
                     Just (show Enums.EXTRA_DISTANCE_FARE),
                     Just (show Enums.CANCELLATION_CHARGES),
                     Just (show Enums.PET_CHARGES),
                     Just (show Enums.LUGGAGE_CHARGE),
                     Just (show Enums.RETURN_FEE),
                     Just (show Enums.BOOTH_CHARGE),
                     Just (show Enums.BUSINESS_DISCOUNT),
                     Just (show Enums.PERSONAL_DISCOUNT),
                     Just (show Enums.PARKING_CHARGE)
                   ]
        _ -> True

mkPaymentParams :: Maybe DMPM.PaymentMethodInfo -> Maybe Text -> Merchant -> DBC.BecknConfig -> DRB.Booking -> Spec.Payment
mkPaymentParams _paymentMethodInfo _paymentUrl merchant bppConfig booking = do
  let mPrice = Just $ Common.mkPrice (Just booking.currency) booking.estimatedFare
  let mkParams :: (Maybe BknPaymentParams) = decodeFromText =<< bppConfig.paymentParamsJson
  mkPayment (show merchant.city) (show bppConfig.collectedBy) Enums.NOT_PAID mPrice booking.paymentId mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee False Nothing

mkPaymentParamsSoftUpdate :: Maybe DMPM.PaymentMethodInfo -> Maybe Text -> Merchant -> DBC.BecknConfig -> HighPrecMoney -> Currency -> Spec.Payment
mkPaymentParamsSoftUpdate _paymentMethodInfo _paymentUrl merchant bppConfig estimatedFare currency = do
  let mPrice = Just $ Common.mkPrice (Just currency) estimatedFare
  let mkParams :: (Maybe BknPaymentParams) = decodeFromText =<< bppConfig.paymentParamsJson
  mkPayment (show merchant.city) (show bppConfig.collectedBy) Enums.NOT_PAID mPrice Nothing mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee False Nothing

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

mkSafetyAlertTags :: Maybe Enums.SafetyReasonCode -> Maybe [Spec.TagGroup]
mkSafetyAlertTags reason =
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
                  { descriptorCode = Just $ show Tags.SAFETY_REASON_CODE,
                    descriptorName = Just "Safety Alert Trigger",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just (T.pack $ maybe "" show reason)
          }

mkUpdatedDistanceTags :: Maybe HighPrecMeters -> Maybe [Spec.TagGroup]
mkUpdatedDistanceTags mbDistance =
  mbDistance >>= \distance ->
    Just
      [ Spec.TagGroup
          { tagGroupDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tags.UPDATE_DETAILS,
                    descriptorName = Just "Update Details",
                    descriptorShortDesc = Nothing
                  },
            tagGroupDisplay = Just True,
            tagGroupList =
              Just $ updatedDistanceSingleton distance
          }
      ]
  where
    updatedDistanceSingleton distance =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tags.UPDATED_ESTIMATED_DISTANCE,
                    descriptorName = Just "Updated Estimated Distance",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just $ show distance
          }

tfItems :: DRide.Ride -> DBooking.Booking -> Utils.MerchantShortId -> Maybe Meters -> Maybe FarePolicyD.FarePolicy -> Maybe Text -> Maybe [Spec.Item]
tfItems ride booking shortId estimatedDistance mbFarePolicy mbPaymentId =
  Just
    [ Spec.Item
        { itemDescriptor = Utils.tfItemDescriptor booking,
          itemFulfillmentIds = Just [ride.id.getId],
          itemId = Just $ maybe (Common.mkItemId shortId booking.vehicleServiceTier) getId (booking.estimateId),
          itemLocationIds = Nothing,
          itemPaymentIds = Utils.tfPaymentId mbPaymentId,
          itemPrice = Utils.tfItemPrice booking.estimatedFare booking.currency,
          itemTags = Utils.mkRateCardTag estimatedDistance booking.fareParams.customerCancellationDues Nothing booking.estimatedFare booking.fareParams.congestionChargeViaDp mbFarePolicy Nothing Nothing
        }
    ]
