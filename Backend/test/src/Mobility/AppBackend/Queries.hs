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
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Geofencing (GeoRestriction (Regions))
import Kernel.Types.Id
import qualified Kernel.Types.Id as K
import Sequelize as Se
import "rider-app" Storage.Beam.Merchant as BeamM

-- updateOrigAndDestRestriction :: Id DM.Merchant -> [Text] -> [Text] -> SqlDB ()
-- updateOrigAndDestRestriction merchantId originList destinationList =
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ MerchantOriginRestriction =. val (Regions originList),
--         MerchantDestinationRestriction =. val (Regions destinationList)
--       ]
--     where_ $ tbl ^. MerchantTId ==. val (toKey merchantId)

updateOrigAndDestRestriction :: MonadFlow m => Id DM.Merchant -> [Text] -> [Text] -> m ()
updateOrigAndDestRestriction (K.Id merchantId) originList destinationList =
  updateWithKV
    [ Se.Set BeamM.originRestriction $ Regions originList,
      Se.Set destinationRestriction $ Regions destinationList
    ]
    [Se.Is BeamM.id $ Se.Eq merchantId]
