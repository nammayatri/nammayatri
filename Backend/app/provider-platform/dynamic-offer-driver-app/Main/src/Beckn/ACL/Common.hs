{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Common where

import qualified Beckn.Types.Core.Taxi.Common.Payment as Payment
import qualified Beckn.Types.Core.Taxi.Common.VehicleVariant as Common
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Vehicle.Variant as Variant

castVariant :: Variant.Variant -> Common.VehicleVariant
castVariant Variant.SEDAN = Common.SEDAN
castVariant Variant.HATCHBACK = Common.HATCHBACK
castVariant Variant.SUV = Common.SUV
castVariant Variant.AUTO_RICKSHAW = Common.AUTO_RICKSHAW
castVariant Variant.TAXI = Common.TAXI
castVariant Variant.TAXI_PLUS = Common.TAXI_PLUS

castPaymentCollector :: DMPM.PaymentCollector -> Payment.PaymentCollector
castPaymentCollector DMPM.BAP = Payment.BAP
castPaymentCollector DMPM.BPP = Payment.BPP

castPaymentType :: DMPM.PaymentType -> Payment.PaymentType
castPaymentType DMPM.PREPAID = Payment.ON_ORDER
castPaymentType DMPM.POSTPAID = Payment.ON_FULFILLMENT

castPaymentInstrument :: DMPM.PaymentInstrument -> Payment.PaymentInstrument
castPaymentInstrument (DMPM.Card DMPM.DefaultCardType) = Payment.Card Payment.DefaultCardType
castPaymentInstrument (DMPM.Wallet DMPM.DefaultWalletType) = Payment.Wallet Payment.DefaultWalletType
castPaymentInstrument DMPM.UPI = Payment.UPI
castPaymentInstrument DMPM.NetBanking = Payment.NetBanking
castPaymentInstrument DMPM.Cash = Payment.Cash
