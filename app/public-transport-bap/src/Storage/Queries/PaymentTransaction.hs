module Storage.Queries.PaymentTransaction where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Booking
import Domain.Types.PaymentTransaction
import Storage.Tabular.PaymentTransaction

findById :: EsqDBFlow m r => Id PaymentTransaction -> m (Maybe PaymentTransaction)
findById paymentTransactionId =
  runTransaction . findOne' $ do
    paymentTransaction <- from $ table @PaymentTransactionT
    where_ $ paymentTransaction ^. PaymentTransactionId ==. val (getId paymentTransactionId)
    return paymentTransaction

findByBookingId :: EsqDBFlow m r => Id Booking -> m (Maybe PaymentTransaction)
findByBookingId bookingId =
  findOne $ do
    parkingSearch <- from $ table @PaymentTransactionT
    where_ $ parkingSearch ^. PaymentTransactionBookingId ==. val (toKey bookingId)
    return parkingSearch

create :: PaymentTransaction -> SqlDB ()
create = create'

updateStatus :: Id PaymentTransaction -> PaymentStatus -> SqlDB ()
updateStatus paymentTransactionId newStatus = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ PaymentTransactionStatus =. val newStatus,
        PaymentTransactionUpdatedAt =. val now
      ]
    where_ $ tbl ^. PaymentTransactionId ==. val (getId paymentTransactionId)

updateTxnDetails :: Id PaymentTransaction -> Text -> PaymentStatus -> SqlDB ()
updateTxnDetails paymentTransactionId paymentGatewayTxnStatus newStatus = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ PaymentTransactionStatus =. val newStatus,
        PaymentTransactionPaymentGatewayTxnStatus =. val paymentGatewayTxnStatus,
        PaymentTransactionUpdatedAt =. val now
      ]
    where_ $ tbl ^. PaymentTransactionId ==. val (getId paymentTransactionId)
