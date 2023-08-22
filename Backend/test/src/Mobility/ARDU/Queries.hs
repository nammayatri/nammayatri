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
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.Id as K
import Sequelize as Se
import qualified Servant.Client as Servant
import "dynamic-offer-driver-app" Storage.Beam.Booking as BeamB

-- updateBapUrl :: BaseUrl -> Id DBooking.Booking -> Esq.SqlDB ()
-- updateBapUrl bapUrl bookingId = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ BookingBapUri =. val (showBaseUrl bapUrl)
--       ]
--     where_ $ tbl ^. BookingTId ==. val (toKey bookingId)

updateBapUrl :: MonadFlow m => BaseUrl -> Id DBooking.Booking -> m ()
updateBapUrl bapUrl (K.Id bookingId) =
  updateWithKV
    [Se.Set bapUri $ showBaseUrl bapUrl]
    [Se.Is BeamB.id $ Se.Eq bookingId]

updateBapUrlWithFake :: MonadFlow m => Id DBooking.Booking -> m ()
updateBapUrlWithFake = updateBapUrl fakeUrl

fakeUrl :: BaseUrl
fakeUrl =
  Servant.BaseUrl
    { baseUrlScheme = Servant.Http,
      baseUrlHost = "fakeUrl",
      baseUrlPort = 80,
      baseUrlPath = ""
    }
