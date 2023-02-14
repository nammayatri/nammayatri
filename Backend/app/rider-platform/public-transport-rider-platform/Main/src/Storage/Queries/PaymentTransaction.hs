 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.PaymentTransaction where

import Domain.Types.Booking
import Domain.Types.PaymentTransaction
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.PaymentTransaction

findById :: Transactionable m => Id PaymentTransaction -> m (Maybe PaymentTransaction)
findById paymentTransactionId =
  Esq.findOne $ do
    paymentTransaction <- from $ table @PaymentTransactionT
    where_ $ paymentTransaction ^. PaymentTransactionId ==. val (getId paymentTransactionId)
    return paymentTransaction

findByBookingId :: Transactionable m => Id Booking -> m (Maybe PaymentTransaction)
findByBookingId bookingId =
  findOne $ do
    parkingSearch <- from $ table @PaymentTransactionT
    where_ $ parkingSearch ^. PaymentTransactionBookingId ==. val (toKey bookingId)
    return parkingSearch

create :: PaymentTransaction -> SqlDB ()
create = Esq.create

updateStatus :: Id PaymentTransaction -> PaymentStatus -> SqlDB ()
updateStatus paymentTransactionId newStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PaymentTransactionStatus =. val newStatus,
        PaymentTransactionUpdatedAt =. val now
      ]
    where_ $ tbl ^. PaymentTransactionId ==. val (getId paymentTransactionId)

updateTxnDetails :: Id PaymentTransaction -> Text -> PaymentStatus -> SqlDB ()
updateTxnDetails paymentTransactionId paymentGatewayTxnStatus newStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PaymentTransactionStatus =. val newStatus,
        PaymentTransactionPaymentGatewayTxnStatus =. val paymentGatewayTxnStatus,
        PaymentTransactionUpdatedAt =. val now
      ]
    where_ $ tbl ^. PaymentTransactionId ==. val (getId paymentTransactionId)
