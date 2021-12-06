{-# LANGUAGE TypeApplications #-}

module Storage.Queries.PaymentTransaction where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.PaymentTransaction
import Storage.Tabular.PaymentTransaction

findById :: EsqDBFlow m r => Id PaymentTransaction -> m (Maybe PaymentTransaction)
findById paymentTransactionId =
  runTransaction . findOne' $ do
    paymentTransaction <- from $ table @PaymentTransactionT
    where_ $ paymentTransaction ^. PaymentTransactionId ==. val (getId paymentTransactionId)
    return paymentTransaction

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
