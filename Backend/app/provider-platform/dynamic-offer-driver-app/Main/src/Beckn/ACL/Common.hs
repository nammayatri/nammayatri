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
                 "RIDE_VAT",
                 "TOLL_VAT"
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
                 "RETURN_FEE",
                 "BOOTH_CHARGE",
                 "RIDE_VAT",
                 "TOLL_VAT"
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
                 "RETURN_FEE",
                 "BOOTH_CHARGE",
                 "RIDE_VAT",
                 "TOLL_VAT"
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
                 "RETURN_FEE",
                 "BOOTH_CHARGE",
                 "RIDE_VAT",
                 "TOLL_VAT"
               ]
    _ -> True

tfContact :: Maybe Text -> Maybe Spec.Contact
tfContact phoneNum =
  Just
    Spec.Contact
      { contactPhone = phoneNum
      }

mkReason :: Maybe Text -> Maybe Text -> Maybe Spec.Reason
mkReason mbCode mbShortDesc =
  mbCode <&> \code ->
    Spec.Reason
      { reasonDescriptor =
          Just $
            Spec.Descriptor
              { descriptorCode = Just code,
                descriptorName = Nothing,
                descriptorShortDesc = mbShortDesc
              }
      }
