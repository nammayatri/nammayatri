module Storage.Queries.BookingCancellationReason where

import Beckn.Storage.Esqueleto as Esq
import Domain.Types.BookingCancellationReason
import Storage.Tabular.BookingCancellationReason ()

create :: BookingCancellationReason -> SqlDB ()
create = Esq.create
