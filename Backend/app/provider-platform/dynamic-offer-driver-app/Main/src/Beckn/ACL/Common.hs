{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Common where

import qualified Beckn.Types.Core.Taxi.Common.BreakupItem as Common
import qualified Beckn.Types.Core.Taxi.Common.CancellationSource as Common
import qualified Beckn.Types.Core.Taxi.Common.Payment as Payment
import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified BecknV2.OnDemand.Types as Spec
import Data.Maybe
import Domain.Types
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Kernel.Prelude

castDPaymentCollector :: DMPM.PaymentCollector -> Payment.PaymentCollector
castDPaymentCollector DMPM.BAP = Payment.BAP
castDPaymentCollector DMPM.BPP = Payment.BPP

castDPaymentType :: DMPM.PaymentType -> Payment.PaymentType
castDPaymentType DMPM.ON_FULFILLMENT = Payment.ON_FULFILLMENT
castDPaymentType DMPM.POSTPAID = Payment.ON_FULFILLMENT

castDPaymentInstrument :: DMPM.PaymentInstrument -> Payment.PaymentInstrument
castDPaymentInstrument (DMPM.Card DMPM.DefaultCardType) = Payment.Card Payment.DefaultCardType
castDPaymentInstrument (DMPM.Wallet DMPM.DefaultWalletType) = Payment.Wallet Payment.DefaultWalletType
castDPaymentInstrument DMPM.UPI = Payment.UPI
castDPaymentInstrument DMPM.NetBanking = Payment.NetBanking
castDPaymentInstrument DMPM.Cash = Payment.Cash
castDPaymentInstrument DMPM.BoothOnline = Payment.BoothOnline

castPaymentCollector :: Payment.PaymentCollector -> DMPM.PaymentCollector
castPaymentCollector Payment.BAP = DMPM.BAP
castPaymentCollector Payment.BPP = DMPM.BPP

castPaymentType :: Payment.PaymentType -> DMPM.PaymentType
castPaymentType Payment.ON_FULFILLMENT = DMPM.ON_FULFILLMENT
castPaymentType Payment.POSTPAID = DMPM.ON_FULFILLMENT

castPaymentInstrument :: Payment.PaymentInstrument -> DMPM.PaymentInstrument
castPaymentInstrument (Payment.Card Payment.DefaultCardType) = DMPM.Card DMPM.DefaultCardType
castPaymentInstrument (Payment.Wallet Payment.DefaultWalletType) = DMPM.Wallet DMPM.DefaultWalletType
castPaymentInstrument Payment.UPI = DMPM.UPI
castPaymentInstrument Payment.NetBanking = DMPM.NetBanking
castPaymentInstrument Payment.Cash = DMPM.Cash
castPaymentInstrument Payment.BoothOnline = DMPM.BoothOnline

makeLocation :: DLoc.Location -> Search.Location
makeLocation DLoc.Location {..} =
  Search.Location
    { gps = Search.Gps {..},
      address =
        Just
          Search.Address
            { area_code = address.areaCode,
              locality = Nothing,
              ward = address.area,
              state = address.state,
              country = address.country,
              building = address.building,
              street = address.street,
              city = address.city,
              door = address.door
            }
    }

mkItemId :: Text -> ServiceTierType -> Text
mkItemId providerId serviceTier = providerId <> "_" <> show serviceTier

type TagGroupCode = Text

type TagCode = Text

getTag :: TagGroupCode -> TagCode -> Tags.TagGroups -> Maybe Text
getTag tagGroupCode tagCode (Tags.TG tagGroups) = do
  tagGroup <- find (\tagGroup -> tagGroup.code == tagGroupCode) tagGroups
  tag <- find (\tag -> tag.code == Just tagCode) tagGroup.list
  tag.value

castCancellationSource :: DBCR.CancellationSource -> Common.CancellationSource
castCancellationSource = \case
  DBCR.ByUser -> Common.ByUser
  DBCR.ByDriver -> Common.ByDriver
  DBCR.ByMerchant -> Common.ByMerchant
  DBCR.ByAllocator -> Common.ByAllocator
  DBCR.ByApplication -> Common.ByApplication
  DBCR.ByFleetOwner -> Common.ByFleetOwner

filterRequiredBreakups :: DFParams.FareParametersType -> Common.BreakupItem -> Bool
filterRequiredBreakups fParamsType breakup = do
  let title = breakup.title
  case fParamsType of
    DFParams.Progressive ->
      title
        `elem` [ "BASE_FARE",
                 "SERVICE_CHARGE",
                 "DEAD_KILOMETER_FARE",
                 "DISTANCE_FARE",
                 "DRIVER_SELECTED_FARE",
                 "CUSTOMER_SELECTED_FARE",
                 "TOTAL_FARE",
                 "WAITING_OR_PICKUP_CHARGES",
                 "EXTRA_TIME_FARE",
                 "CANCELLATION_CHARGES",
                 "PARKING_CHARGE",
                 "NIGHT_SHIFT_CHARGE",
                 "RIDE_STOP_CHARGES",
                 "PER_STOP_CHARGES",
                 "LUGGAGE_CHARGE",
                 "RETURN_FEE",
                 "BOOTH_CHARGE",
                 "DRIVER_ALLOWANCE",
                 "AIRPORT_CONVENIENCE_FEE",
                 "RIDE_VAT",
                 "TOLL_VAT",
                 "TOLL_FARE_TAX_EXCLUSIVE",
                 "TOLL_FARE_TAX",
                 "RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE",
                 "RIDE_FARE_DISCOUNT_APPLICABLE_TAX",
                 "RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE",
                 "RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX",
                 "CANCELLATION_FEE_TAX_EXCLUSIVE",
                 "CANCELLATION_TAX",
                 "PARKING_CHARGE_TAX_EXCLUSIVE",
                 "PARKING_CHARGE_TAX"
               ]
    DFParams.Slab ->
      title
        `elem` [ "BASE_FARE",
                 "SERVICE_CHARGE",
                 "WAITING_OR_PICKUP_CHARGES",
                 "PLATFORM_FEE",
                 "SGST",
                 "CGST",
                 "FIXED_GOVERNMENT_RATE",
                 "CUSTOMER_SELECTED_FARE",
                 "TOTAL_FARE",
                 "NIGHT_SHIFT_CHARGE",
                 "EXTRA_TIME_FARE",
                 "CANCELLATION_CHARGES",
                 "PARKING_CHARGE",
                 "LUGGAGE_CHARGE",
                 "DRIVER_ALLOWANCE",
                 "AIRPORT_CONVENIENCE_FEE",
                 "RETURN_FEE",
                 "BOOTH_CHARGE",
                 "RIDE_VAT",
                 "TOLL_VAT",
                 "TOLL_FARE_TAX_EXCLUSIVE",
                 "TOLL_FARE_TAX",
                 "RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE",
                 "RIDE_FARE_DISCOUNT_APPLICABLE_TAX",
                 "RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE",
                 "RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX",
                 "CANCELLATION_FEE_TAX_EXCLUSIVE",
                 "CANCELLATION_TAX",
                 "PARKING_CHARGE_TAX_EXCLUSIVE",
                 "PARKING_CHARGE_TAX"
               ]
    DFParams.Rental ->
      title
        `elem` [ "BASE_FARE",
                 "SERVICE_CHARGE",
                 "DEAD_KILOMETER_FARE",
                 "DIST_BASED_FARE",
                 "TIME_BASED_FARE",
                 "NIGHT_SHIFT_CHARGE",
                 "DRIVER_SELECTED_FARE",
                 "CUSTOMER_SELECTED_FARE",
                 "TOTAL_FARE",
                 "WAITING_OR_PICKUP_CHARGES",
                 "EXTRA_TIME_FARE",
                 "CANCELLATION_CHARGES",
                 "PARKING_CHARGE",
                 "LUGGAGE_CHARGE",
                 "DRIVER_ALLOWANCE",
                 "AIRPORT_CONVENIENCE_FEE",
                 "RETURN_FEE",
                 "BOOTH_CHARGE",
                 "RIDE_VAT",
                 "TOLL_VAT",
                 "TOLL_FARE_TAX_EXCLUSIVE",
                 "TOLL_FARE_TAX",
                 "RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE",
                 "RIDE_FARE_DISCOUNT_APPLICABLE_TAX",
                 "RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE",
                 "RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX",
                 "CANCELLATION_FEE_TAX_EXCLUSIVE",
                 "CANCELLATION_TAX",
                 "PARKING_CHARGE_TAX_EXCLUSIVE",
                 "PARKING_CHARGE_TAX"
               ]
    DFParams.InterCity ->
      title
        `elem` [ "BASE_FARE",
                 "SERVICE_CHARGE",
                 "DEAD_KILOMETER_FARE",
                 "DIST_BASED_FARE",
                 "TIME_BASED_FARE",
                 "NIGHT_SHIFT_CHARGE",
                 "DRIVER_SELECTED_FARE",
                 "CUSTOMER_SELECTED_FARE",
                 "TOTAL_FARE",
                 "WAITING_OR_PICKUP_CHARGES",
                 "EXTRA_TIME_FARE",
                 "EXTRA_DISTANCE_FARE",
                 "CANCELLATION_CHARGES",
                 "PARKING_CHARGE",
                 "LUGGAGE_CHARGE",
                 "DRIVER_ALLOWANCE",
                 "AIRPORT_CONVENIENCE_FEE",
                 "RETURN_FEE",
                 "BOOTH_CHARGE",
                 "RIDE_VAT",
                 "TOLL_VAT",
                 "TOLL_FARE_TAX_EXCLUSIVE",
                 "TOLL_FARE_TAX",
                 "RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE",
                 "RIDE_FARE_DISCOUNT_APPLICABLE_TAX",
                 "RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE",
                 "RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX",
                 "CANCELLATION_FEE_TAX_EXCLUSIVE",
                 "CANCELLATION_TAX",
                 "PARKING_CHARGE_TAX_EXCLUSIVE",
                 "PARKING_CHARGE_TAX"
               ]
    _ -> True

tfContact :: Maybe Text -> Maybe Spec.Contact
tfContact phoneNum =
  Just
    Spec.Contact
      { contactPhone = phoneNum
      }

-- Emits a descriptor when *either* field is present. Reasons with no honest ONDC code still
-- need to carry their free-text description, so keying this on the code alone would silence them.
mkReason :: Maybe Text -> Maybe Text -> Maybe Spec.Reason
mkReason mbCode mbShortDesc =
  if isNothing mbCode && isNothing mbShortDesc
    then Nothing
    else
      Just $
        Spec.Reason
          { reasonDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = mbCode,
                    descriptorName = Nothing,
                    descriptorShortDesc = mbShortDesc,
                    descriptorLongDesc = Nothing
                  }
          }

-- | Translate an internal cancellation reason code into what ONDC puts on the wire.
--
-- ONDC's seller enum is closed: 011 NO_DRIVERS_AVAILABLE, 012 COULD_NOT_FIND_CUSTOMER,
-- 013 RIDE_ACCEPTED_MISTAKENLY, 014 UNABLE_TO_CONTACT_RIDER. Only three of our reasons have an
-- honest equivalent. The rest omit the code rather than assert something false — a buyer app may
-- surface it to the rider, and @cancellationCancelledBy = PROVIDER@ already conveys who cancelled.
--
-- The short_desc is restricted to reasons we recognise, so unvalidated values cannot reach the
-- wire (production has carried `aliquip` and `string` in this field). English by design: the
-- rider's language is the buyer app's concern, and it localises from the code.
ondcCancellationReason :: Text -> (Maybe Text, Maybe Text)
ondcCancellationReason = \case
  "DRIVER_CANCEL_CUSTOMER_NO_SHOW" -> (Just "012", Just "Customer did not show up")
  "DRIVER_CANCEL_PASSENGER_UNREACHABLE" -> (Just "014", Just "Passenger unreachable after multiple attempts")
  "DRIVER_CANCEL_INVALID_BOOKING" -> (Just "013", Just "Incorrect, duplicate or cancelled booking")
  "DRIVER_CANCEL_SAFETY_OR_MISCONDUCT" -> (Nothing, Just "Safety concern or passenger misconduct")
  "DRIVER_CANCEL_UNSAFE_RIDE_REQUEST" -> (Nothing, Just "Unsafe or non-compliant ride request")
  "DRIVER_CANCEL_EMERGENCY_OR_UNFORESEEN" -> (Nothing, Just "Accident, medical emergency or unforeseen event")
  "DRIVER_CANCEL_VEHICLE_BREAKDOWN" -> (Nothing, Just "Vehicle breakdown or mechanical issue")
  "DRIVER_CANCEL_LOCATION_INACCESSIBLE" -> (Nothing, Just "Pickup or drop location inaccessible")
  _ -> (Nothing, Nothing)

-- | Resolve what goes into @order.cancellation.reason@ for a merchant.
--
-- Deliberately *not* keyed on @value_add_np@: that marks BAPs which have integrated our private tag
-- extensions, which is a different property from parsing our private cancellation vocabulary. MSIL
-- needs to be value-add (reallocation, tracking url, fulfillment tags) while still needing
-- spec-valid codes, so the two must be decided independently.
--
-- Off (the default) reproduces today's payload byte-for-byte: the internal code in both fields on
-- the on_status path, and code-only on on_cancel. Callers pass their own legacy shape as
-- @mbLegacyShortDesc@ so this stays the single decision point.
mkCancellationReason :: Bool -> Maybe Text -> Maybe Text -> Maybe Spec.Reason
mkCancellationReason sendOndcCodes mbInternalCode mbLegacyShortDesc
  | not sendOndcCodes = mkReason mbInternalCode mbLegacyShortDesc
  | otherwise = case ondcCancellationReason <$> mbInternalCode of
    Nothing -> Nothing
    Just (mbCode, mbShortDesc) -> mkReason mbCode mbShortDesc
