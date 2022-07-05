module Storage.Queries.DriverQuote where

import Beckn.Storage.Esqueleto as Esq
import qualified Domain.Types.DriverQuote as Domain
import Storage.Tabular.DriverQuote ()

create :: Domain.DriverQuote -> SqlDB ()
create = Esq.create
