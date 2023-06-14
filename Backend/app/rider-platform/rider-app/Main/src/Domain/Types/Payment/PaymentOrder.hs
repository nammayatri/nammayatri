{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Payment.PaymentOrder where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data PaymentOrderE e = PaymentOrder
  { id :: Id PaymentOrder, -- can be same as ride.id
    shortId :: ShortId PaymentOrder, -- can be same as ride.shortId
    customerId :: Id DP.Person,
    merchantId :: Id DM.Merchant,
    amount :: Money,
    currency :: Payment.Currency,
    status :: Payment.TransactionStatus,
    paymentLinks :: Payment.PaymentLinks,
    clientAuthToken :: EncryptedHashedField e Text,
    clientAuthTokenExpiry :: UTCTime,
    getUpiDeepLinksOption :: Maybe Bool,
    environment :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type PaymentOrder = PaymentOrderE 'AsEncrypted
