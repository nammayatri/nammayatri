module Storage.Queries.DiscountTransaction where

import Domain.Types.DiscountTransaction
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.DiscountTransaction ()

create :: DiscountTransaction -> SqlDB ()
create = Esq.create
