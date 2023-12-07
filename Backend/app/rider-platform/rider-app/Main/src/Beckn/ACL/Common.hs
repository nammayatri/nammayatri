{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Common where

import qualified Beckn.Types.Core.Taxi.Common.Descriptor as Descriptor
import qualified Beckn.Types.Core.Taxi.Common.Payment as Payment
import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.Common.Vehicle as Common
import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.VehicleVariant as Variant
import Kernel.Prelude
import Kernel.Utils.Common
import Tools.Error

validatePrices :: (MonadThrow m, Log m, Num a, Ord a) => a -> a -> m ()
validatePrices price priceWithDiscount = do
  when (price < 0) $ throwError $ InvalidRequest "price is less than zero"
  when (priceWithDiscount < 0) $ throwError $ InvalidRequest "discounted price is less than zero"
  when (priceWithDiscount > price) $ throwError $ InvalidRequest "price is lesser than discounted price"

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
castPaymentType Payment.PRE_ORDER = DMPM.PREPAID
castPaymentType Payment.PRE_FULFILLMENT = DMPM.PREPAID
castPaymentType Payment.POST_FULFILLMENT = DMPM.POSTPAID

castPaymentInstrument :: Payment.PaymentInstrument -> DMPM.PaymentInstrument
castPaymentInstrument (Payment.Card Payment.DefaultCardType) = DMPM.Card DMPM.DefaultCardType
castPaymentInstrument (Payment.Wallet Payment.DefaultWalletType) = DMPM.Wallet DMPM.DefaultWalletType
castPaymentInstrument Payment.UPI = DMPM.UPI
castPaymentInstrument Payment.NetBanking = DMPM.NetBanking
castPaymentInstrument Payment.Cash = DMPM.Cash

mkLocation :: DSearchCommon.SearchReqLocation -> Search.Location
mkLocation info =
  Search.Location
    { gps =
        Search.Gps
          { lat = info.gps.lat,
            lon = info.gps.lon
          },
      address =
        Just
          Search.Address
            { locality = info.address.area,
              state = info.address.state,
              country = info.address.country,
              building = info.address.building,
              street = info.address.street,
              city = info.address.city,
              area_code = info.address.areaCode,
              door = info.address.door,
              ward = info.address.ward
            }
    }

castVariant :: Variant.VehicleVariant -> Common.VehicleVariant
castVariant Variant.SEDAN = Common.SEDAN
castVariant Variant.HATCHBACK = Common.HATCHBACK
castVariant Variant.SUV = Common.SUV
castVariant Variant.AUTO_RICKSHAW = Common.AUTO_RICKSHAW
castVariant Variant.TAXI = Common.TAXI
castVariant Variant.TAXI_PLUS = Common.TAXI_PLUS

type TagGroupCode = Text

type TagCode = Text

getTag :: TagGroupCode -> TagCode -> Tags.TagGroups -> Maybe Text
getTag tagGroupCode tagCode (Tags.TG tagGroups) = do
  tagGroup <- find (\tagGroup -> tagGroup.code == tagGroupCode) tagGroups
  tag <- find (\tag -> tag.code == Just tagCode) tagGroup.list
  tag.value

getTagV2 :: TagGroupCode -> TagCode -> [Tags.TagGroupV2] -> Maybe Text
getTagV2 tagGroupCode tagCode tagGroups = do
  tagGroup <- find (\tagGroup -> tagGroup.descriptor.code == tagGroupCode) tagGroups
  tag <- find (\tag -> descriptorCode tag.descriptor == Just tagCode) tagGroup.list
  tag.value
  where
    descriptorCode :: Maybe Descriptor.DescriptorV2 -> Maybe Text
    descriptorCode (Just desc) = Just desc.code
    descriptorCode Nothing = Nothing
