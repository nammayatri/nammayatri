{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.AppBackend.Queries where

import qualified "rider-app" Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (findById)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Geofencing (GeoRestriction (Regions))
import Kernel.Types.Id
import "rider-app" Storage.Tabular.Merchant

updateOrigAndDestRestriction :: Id DM.Merchant -> [Text] -> [Text] -> SqlDB m ()
updateOrigAndDestRestriction merchantId originList destinationList =
  Esq.update $ \tbl -> do
    set
      tbl
      [ MerchantOriginRestriction =. val (Regions originList),
        MerchantDestinationRestriction =. val (Regions destinationList)
      ]
    where_ $ tbl ^. MerchantTId ==. val (toKey merchantId)
