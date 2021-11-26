{-# LANGUAGE TypeApplications #-}

module Storage.Queries.PaymentTransaction where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Storage.Domain.PaymentTransaction as Domain

findById :: (Esq.EsqDBFlow m r, HasLog r) => Id Domain.PaymentTransaction -> m (Maybe Domain.PaymentTransaction)
findById parkingSearchId =
  Esq.runTransaction . Esq.findOne' $ do
    parkingSearch <- Esq.from $ Esq.table @Domain.PaymentTransactionT
    Esq.where_ $ parkingSearch Esq.^. Domain.PaymentTransactionTId Esq.==. Esq.val (Domain.PaymentTransactionTKey $ getId parkingSearchId)
    return parkingSearch

create :: Domain.PaymentTransaction -> SqlDB Domain.PaymentTransaction
create = Esq.createReturningEntity'

updateStatus :: Domain.PaymentTransaction -> Domain.PaymentStatus -> SqlDB ()
updateStatus paymentTransaction newStatus = do
  now <- getCurrentTime
  Esq.update' $ \tbl -> do
    Esq.set
      tbl
      [ Domain.PaymentTransactionTStatus Esq.=. Esq.val newStatus,
        Domain.PaymentTransactionTUpdatedAt Esq.=. Esq.val now
      ]
    Esq.where_ $ tbl Esq.^. Domain.PaymentTransactionTId Esq.==. Esq.val (toKey paymentTransaction)
