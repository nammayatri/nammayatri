module Storage.Queries.PaymentTransaction where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Booking
import Domain.PaymentTransaction
import Storage.Tabular.PaymentTransaction

findById :: Transactionable m => Id PaymentTransaction -> m (Maybe PaymentTransaction)
findById = Esq.findById

findByBookingId :: Transactionable m => Id Booking -> m (Maybe PaymentTransaction)
findByBookingId bookingId =
  findOne $ do
    parkingSearch <- from $ table @PaymentTransactionT
    where_ $ parkingSearch ^. PaymentTransactionBookingId ==. val (toKey bookingId)
    return parkingSearch

create :: PaymentTransaction -> SqlDB ()
create = Esq.create

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
