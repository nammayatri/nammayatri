{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.Common where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.FRFSQuoteCategoryType
import Kernel.Prelude
import Kernel.Utils.Common

data DCategorySelect = DCategorySelect
  { bppItemId :: Text,
    quantity :: Int,
    category :: FRFSQuoteCategoryType,
    price :: Price
  }

data DOnSelect = DOnSelect
  { providerId :: Text,
    fareBreakUp :: [DFareBreakUp],
    validTill :: Maybe UTCTime,
    transactionId :: Text,
    messageId :: Text,
    categories :: [DCategorySelect]
  }

data DFareBreakUp = DFareBreakUp
  { title :: Text,
    price :: HighPrecMoney,
    pricePerUnit :: HighPrecMoney,
    quantity :: Int
  }

data DOrder = DOrder
  { providerId :: Text,
    totalPrice :: HighPrecMoney,
    fareBreakUp :: [DFareBreakUp],
    bppOrderId :: Text,
    bppItemId :: Text,
    transactionId :: Text,
    orderStatus :: Maybe Spec.OrderStatus,
    messageId :: Text,
    tickets :: [DTicket]
  }

data DTicket = DTicket
  { qrData :: Text,
    vehicleNumber :: Maybe Text,
    description :: Maybe Text,
    bppFulfillmentId :: Maybe Text,
    ticketNumber :: Text,
    validTill :: UTCTime,
    status :: Text,
    qrRefreshAt :: Maybe UTCTime,
    commencingHours :: Maybe Int,
    isReturnTicket :: Maybe Bool
  }

data DTicketPayload = DTicketPayload
  { fromRouteProviderCode :: Text,
    toRouteProviderCode :: Text,
    adultQuantity :: Int,
    childQuantity :: Int,
    vehicleTypeProviderCode :: Text,
    expiry :: Text,
    ticketNumber :: Text,
    ticketAmount :: Money,
    refreshAt :: Maybe UTCTime
  }
