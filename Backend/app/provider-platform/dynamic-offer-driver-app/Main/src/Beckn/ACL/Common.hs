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
import qualified Beckn.Types.Core.Taxi.Common.Vehicle as Common
import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified BecknV2.OnDemand.Types as Spec
import Data.Maybe
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Common as DCT
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude

castVariant :: Variant.Variant -> Common.VehicleVariant
castVariant Variant.SEDAN = Common.SEDAN
castVariant Variant.HATCHBACK = Common.HATCHBACK
castVariant Variant.SUV = Common.SUV
castVariant Variant.AUTO_RICKSHAW = Common.AUTO_RICKSHAW
castVariant Variant.TAXI = Common.TAXI
castVariant Variant.TAXI_PLUS = Common.TAXI_PLUS

castDPaymentCollector :: DMPM.PaymentCollector -> Payment.PaymentCollector
castDPaymentCollector DMPM.BAP = Payment.BAP
castDPaymentCollector DMPM.BPP = Payment.BPP

castDPaymentType :: DMPM.PaymentType -> Payment.PaymentType
castDPaymentType DMPM.PREPAID = Payment.ON_ORDER
castDPaymentType DMPM.POSTPAID = Payment.ON_FULFILLMENT

castDPaymentInstrument :: DMPM.PaymentInstrument -> Payment.PaymentInstrument
castDPaymentInstrument (DMPM.Card DMPM.DefaultCardType) = Payment.Card Payment.DefaultCardType
castDPaymentInstrument (DMPM.Wallet DMPM.DefaultWalletType) = Payment.Wallet Payment.DefaultWalletType
castDPaymentInstrument DMPM.UPI = Payment.UPI
castDPaymentInstrument DMPM.NetBanking = Payment.NetBanking
castDPaymentInstrument DMPM.Cash = Payment.Cash

castPaymentCollector :: Payment.PaymentCollector -> DMPM.PaymentCollector
castPaymentCollector Payment.BAP = DMPM.BAP
castPaymentCollector Payment.BPP = DMPM.BPP

castPaymentType :: Payment.PaymentType -> DMPM.PaymentType
castPaymentType Payment.ON_ORDER = DMPM.PREPAID
castPaymentType Payment.ON_FULFILLMENT = DMPM.POSTPAID

castPaymentInstrument :: Payment.PaymentInstrument -> DMPM.PaymentInstrument
castPaymentInstrument (Payment.Card Payment.DefaultCardType) = DMPM.Card DMPM.DefaultCardType
castPaymentInstrument (Payment.Wallet Payment.DefaultWalletType) = DMPM.Wallet DMPM.DefaultWalletType
castPaymentInstrument Payment.UPI = DMPM.UPI
castPaymentInstrument Payment.NetBanking = DMPM.NetBanking
castPaymentInstrument Payment.Cash = DMPM.Cash

makeLocation :: DLoc.Location -> Search.Location
makeLocation DLoc.Location {..} =
  Search.Location
    { gps = Search.Gps {..},
      address =
        Just
          Search.Address
            { area_code = address.areaCode,
              locality = address.area,
              ward = address.area,
              state = address.state,
              country = address.country,
              building = address.building,
              street = address.street,
              city = address.city,
              door = address.door
            }
    }

mkItemId :: Text -> Variant.Variant -> Text
mkItemId providerId variant = providerId <> "_" <> show variant

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

filterRequiredBreakups :: DFParams.FareParametersType -> Common.BreakupItem -> Bool
filterRequiredBreakups fParamsType breakup = do
  let title = breakup.title
  case fParamsType of
    DFParams.Progressive ->
      title
        `elem` [ "BASE_FARE",
                 "SERVICE_CHARGE",
                 "DEAD_KILOMETER_FARE",
                 "EXTRA_DISTANCE_FARE",
                 "DRIVER_SELECTED_FARE",
                 "CUSTOMER_SELECTED_FARE",
                 "TOTAL_FARE",
                 "WAITING_OR_PICKUP_CHARGES",
                 "EXTRA_TIME_FARE",
                 "CUSTOMER_CANCELLATION_DUES"
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
                 "CUSTOMER_CANCELLATION_DUES"
               ]
    DFParams.Rental ->
      title
        `elem` [ "BASE_FARE",
                 "SERVICE_CHARGE",
                 "DEAD_KILOMETER_FARE",
                 "DIST_BASED_FARE",
                 "TIME_BASED_FARE",
                 "DRIVER_SELECTED_FARE",
                 "CUSTOMER_SELECTED_FARE",
                 "TOTAL_FARE",
                 "WAITING_OR_PICKUP_CHARGES",
                 "EXTRA_TIME_FARE",
                 "CUSTOMER_CANCELLATION_DUES"
               ]

mkFulfillmentType :: DCT.TripCategory -> Text
mkFulfillmentType = \case
  DCT.OneWay DCT.OneWayRideOtp -> "RIDE_OTP"
  DCT.RoundTrip DCT.RideOtp -> "RIDE_OTP"
  DCT.RideShare DCT.RideOtp -> "RIDE_OTP"
  DCT.Rental DCT.RideOtp -> "RIDE_OTP"
  _ -> "RIDE"

getTagV2 :: TagGroupCode -> TagCode -> [Spec.TagGroup] -> Maybe Text
getTagV2 tagGroupCode tagCode tagGroups = do
  tagGroup <- find (\tagGroup -> descriptorCode tagGroup.tagGroupDescriptor == Just tagGroupCode) tagGroups
  case tagGroup.tagGroupList of
    Nothing -> Nothing
    Just tagGroupList -> do
      tag <- find (\tag -> descriptorCode tag.tagDescriptor == Just tagCode) tagGroupList
      tag.tagValue
  where
    descriptorCode :: Maybe Spec.Descriptor -> Maybe Text
    descriptorCode (Just desc) = desc.descriptorCode
    descriptorCode Nothing = Nothing
