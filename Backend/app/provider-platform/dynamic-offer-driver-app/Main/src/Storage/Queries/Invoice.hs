module Storage.Queries.Invoice where

import Domain.Types.Invoice
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Invoice

createMany :: [Invoice] -> SqlDB ()
createMany = Esq.createMany

findAllByInvoiceId ::
  Transactionable m =>
  Id Invoice ->
  m [Invoice]
findAllByInvoiceId invoiceId = do
  Esq.findAll $ do
    invoice <- from $ table @InvoiceT
    where_ $
      invoice ^. InvoiceId ==. val (invoiceId.getId)
    return invoice
