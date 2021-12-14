module Storage.Queries.PaymentTransaction where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Booking
import Domain.PaymentTransaction
import Storage.Tabular.PaymentTransaction

findById :: EsqDBFlow m r => Id PaymentTransaction -> m (Maybe PaymentTransaction)
findById = Esq.findById

findByBookingId :: EsqDBFlow m r => Id Booking -> m (Maybe PaymentTransaction)
findByBookingId bookingId =
  findOne $ do
    parkingSearch <- from $ table @PaymentTransactionT
    where_ $ parkingSearch ^. PaymentTransactionBookingId ==. val (toKey bookingId)
    return parkingSearch

create :: PaymentTransaction -> SqlDB ()
create = create'

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
