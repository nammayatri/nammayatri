module Storage.Queries.RideBookingCancellationReason where

import Beckn.Storage.Esqueleto as Esq
import Domain.Types.RideBookingCancellationReason
import Storage.Tabular.RideBookingCancellationReason ()

create :: RideBookingCancellationReason -> SqlDB ()
create = Esq.create'
