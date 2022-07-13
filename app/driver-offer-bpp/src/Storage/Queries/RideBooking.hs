module Storage.Queries.RideBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Domain.Types.RideBooking
import Storage.Tabular.RideBooking ()

create :: RideBooking -> SqlDB ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' sReq
