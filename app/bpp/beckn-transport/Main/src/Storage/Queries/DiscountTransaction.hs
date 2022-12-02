module Storage.Queries.DiscountTransaction where

import Beckn.Storage.Esqueleto as Esq
import Domain.Types.DiscountTransaction
import Storage.Tabular.DiscountTransaction ()

create :: DiscountTransaction -> SqlDB ()
create = Esq.create
