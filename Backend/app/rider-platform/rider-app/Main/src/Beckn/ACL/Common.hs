{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Common where

import qualified Beckn.Types.Core.Taxi.Common.Payment as Common
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import Kernel.Prelude
import Kernel.Utils.Common
import Tools.Error

validatePrices :: (MonadThrow m, Log m, Num a, Ord a) => a -> a -> m ()
validatePrices price priceWithDiscount = do
  when (price < 0) $ throwError $ InvalidRequest "price is less than zero"
  when (priceWithDiscount < 0) $ throwError $ InvalidRequest "discounted price is less than zero"
  when (priceWithDiscount > price) $ throwError $ InvalidRequest "price is lesser than discounted price"

castPaymentCollector :: Common.PaymentCollector -> DMPM.PaymentCollector
castPaymentCollector Common.BAP = DMPM.BAP
castPaymentCollector Common.BPP = DMPM.BPP

castPaymentType :: Common.PaymentType -> DMPM.PaymentType
castPaymentType Common.ON_ORDER = DMPM.PREPAID
castPaymentType Common.ON_FULFILLMENT = DMPM.POSTPAID

castPaymentInstrument :: Common.PaymentInstrument -> DMPM.PaymentInstrument
castPaymentInstrument (Common.Card Common.DefaultCardType) = DMPM.Card DMPM.DefaultCardType
castPaymentInstrument (Common.Wallet Common.DefaultWalletType) = DMPM.Wallet DMPM.DefaultWalletType
castPaymentInstrument Common.UPI = DMPM.UPI
castPaymentInstrument Common.NetBanking = DMPM.NetBanking
castPaymentInstrument Common.Cash = DMPM.Cash
