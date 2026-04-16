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
import BecknV2.OnDemand.Tags ((~=), (~=?))
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.OnDemand.Utils.Payment
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
import qualified Kernel.Types.Price
import Kernel.Utils.Common hiding (mkPrice)
import SharedLogic.FareCalculator as Fare
import Tools.Error

-- TODO::Beckn, `Payment.ON_ORDER` Not present in spec.
mkRideCompletedPaymentType :: Maybe DMPM.PaymentMethodInfo -> Text
mkRideCompletedPaymentType = show . maybe OnUpdate.ON_FULFILLMENT (Common.castDPaymentType . (.paymentType))

showPaymentCollectedBy :: Maybe DMPM.PaymentMethodInfo -> Text
showPaymentCollectedBy = show . maybe OnUpdate.BPP (Common.castDPaymentCollector . (.collectedBy))

mkRideCompletedQuote :: MonadFlow m => DRide.Ride -> DFParams.FareParameters -> HighPrecMoney -> m Spec.Quotation
mkRideCompletedQuote ride fareParams estimatedFare = do
  let fare' = estimatedFare
      roundedFare = Kernel.Types.Price.showPriceWithRoundingWithoutCurrency $ Kernel.Types.Price.mkPrice (Just ride.currency) fare'
  let currency = show ride.currency
      price =
        Spec.Price
          { priceCurrency = Just currency,
            priceValue = Just roundedFare,
            priceComputedValue = Nothing,
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Nothing
          }
      breakup =
        let rawBreakups = Fare.mkFareParamsBreakups (\x -> x) (\title money -> (title, money)) fareParams
            normalized = mapMaybe (\(title, money) -> (,money) <$> Utils.normalizeBreakupTitle title) rawBreakups
         in Utils.aggregateBreakupsWithTotal (Just fare') fareParams.currency normalized
  pure
    Spec.Quotation
      { quotationBreakup = Just breakup,
        quotationPrice = Just price,
        quotationTtl = Nothing
      }

mkPaymentParams :: Maybe DMPM.PaymentMethodInfo -> Maybe Text -> Merchant -> DBC.BecknConfig -> DRB.Booking -> Spec.Payment
mkPaymentParams _paymentMethodInfo _paymentUrl merchant bppConfig booking = do
  let mPrice = Just $ Common.mkPrice (Just booking.currency) booking.estimatedFare
  let mkParams :: (Maybe BknPaymentParams) = decodeFromText =<< bppConfig.paymentParamsJson
  mkPayment (show merchant.city) (show bppConfig.collectedBy) Enums.NOT_PAID mPrice booking.paymentId mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee False Nothing Nothing

mkPaymentParamsSoftUpdate :: Maybe DMPM.PaymentMethodInfo -> Maybe Text -> Merchant -> DBC.BecknConfig -> HighPrecMoney -> Currency -> Spec.Payment
mkPaymentParamsSoftUpdate _paymentMethodInfo _paymentUrl merchant bppConfig estimatedFare currency = do
  let mPrice = Just $ Common.mkPrice (Just currency) estimatedFare
  let mkParams :: (Maybe BknPaymentParams) = decodeFromText =<< bppConfig.paymentParamsJson
  mkPayment (show merchant.city) (show bppConfig.collectedBy) Enums.NOT_PAID mPrice Nothing mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee False Nothing Nothing

mkDistanceTagGroup :: MonadFlow m => DRide.Ride -> m (Maybe [Spec.TagGroup])
mkDistanceTagGroup ride = do
  chargeableDistance :: HighPrecMeters <-
    realToFrac <$> ride.chargeableDistance
      & fromMaybeM (InternalError "Ride chargeable distance is not present in OnUpdateBuildReq ride.")
  let traveledDistance :: HighPrecMeters = ride.traveledDistance
  let endOdometerReading :: Maybe Centesimal = (.value) <$> ride.endOdometerReading
  pure $
    Tags.buildTagGroups
      [ Tags.CHARGEABLE_DISTANCE ~= show chargeableDistance,
        Tags.TRAVELED_DISTANCE ~= show traveledDistance,
        Tags.END_ODOMETER_READING ~=? (show <$> endOdometerReading)
      ]

mkPreviousCancellationReasonsTags :: SBCR.CancellationSource -> Maybe [Spec.TagGroup]
mkPreviousCancellationReasonsTags cancellationSource =
  Tags.buildTagGroups
    [ Tags.CANCELLATION_REASON ~= show (castCancellationSource cancellationSource)
    ]

castCancellationSource :: SBCR.CancellationSource -> BookingCancelledOU.CancellationSource
castCancellationSource = \case
  SBCR.ByUser -> BookingCancelledOU.ByUser
  SBCR.ByDriver -> BookingCancelledOU.ByDriver
  SBCR.ByMerchant -> BookingCancelledOU.ByMerchant
  SBCR.ByAllocator -> BookingCancelledOU.ByAllocator
  SBCR.ByApplication -> BookingCancelledOU.ByApplication
  SBCR.ByFleetOwner -> BookingCancelledOU.ByFleetOwner

mkNewMessageTags :: Text -> Maybe [Spec.TagGroup]
mkNewMessageTags message =
  Tags.buildTagGroups
    [ Tags.MESSAGE ~= message
    ]

mkSafetyAlertTags :: Maybe Enums.SafetyReasonCode -> Maybe [Spec.TagGroup]
mkSafetyAlertTags reason =
  Tags.buildTagGroups
    [ Tags.SAFETY_REASON_CODE ~= T.pack (maybe "" show reason)
    ]

mkUpdatedDistanceTags :: Maybe HighPrecMeters -> Maybe [Spec.TagGroup]
mkUpdatedDistanceTags = Tags.mkSingleTagGroup Tags.UPDATED_ESTIMATED_DISTANCE

tfItems :: DRide.Ride -> DBooking.Booking -> Utils.MerchantShortId -> Maybe Meters -> Maybe FarePolicyD.FarePolicy -> Maybe Text -> Maybe [Spec.Item]
tfItems _ride booking shortId _estimatedDistance mbFarePolicy mbPaymentId =
  let farePolicyTags = maybeToList (Utils.mkSpecFarePolicyTagsFromPolicy mbFarePolicy)
      infoTags = fromMaybe [] (Utils.mkInfoTagGroup booking.distanceToPickup booking.dqDurationToPickup)
      featureList = Utils.mkFeatureListTags booking.isAirConditioned
      combinedTags = Just $ farePolicyTags <> infoTags <> [featureList]
   in Just
        [ Spec.Item
            { itemCancellationTerms = Nothing,
              itemDescriptor = Utils.tfItemDescriptor booking,
              itemFulfillmentIds = Just [Utils.getBookingFulfillmentId booking],
              itemId = Just $ maybe (Common.mkItemId shortId booking.vehicleServiceTier) getId (booking.estimateId),
              itemCategoryIds = Just [Utils.tripCategoryToCategoryCode booking.tripCategory],
              itemLocationIds = Nothing,
              itemPaymentIds = Utils.tfPaymentId mbPaymentId,
              itemPrice = Utils.tfItemPrice booking.estimatedFare booking.currency,
              itemTags = combinedTags
            }
        ]
