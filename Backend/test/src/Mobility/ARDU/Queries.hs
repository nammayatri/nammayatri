{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.ARDU.Queries where

import qualified "dynamic-offer-driver-app" Domain.Types.Booking as DBooking
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Servant.Client as Servant
import "dynamic-offer-driver-app" Storage.Tabular.Booking

updateBapUrl :: BaseUrl -> Id DBooking.Booking -> Esq.SqlDB m ()
updateBapUrl bapUrl bookingId = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingBapUri =. val (showBaseUrl bapUrl)
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey bookingId)

updateBapUrlWithFake :: Id DBooking.Booking -> Esq.SqlDB m ()
updateBapUrlWithFake = updateBapUrl fakeUrl

fakeUrl :: BaseUrl
fakeUrl =
  Servant.BaseUrl
    { baseUrlScheme = Servant.Http,
      baseUrlHost = "fakeUrl",
      baseUrlPort = 80,
      baseUrlPath = ""
    }
