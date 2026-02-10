{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Queries.GateInfoGeom where

import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Lib.Tabular.GateInfoGeom
import qualified Lib.Types.GateInfo as D

create :: D.GateInfo -> SqlDB ()
create = Esq.create

updateGate :: D.GateInfo -> SqlDB ()
updateGate D.GateInfo {..} = Esq.update $ \tbl -> do
  let merchantOperatingCityId' = Kernel.Types.Id.getId <$> merchantOperatingCityId
      merchantId' = Kernel.Types.Id.getId <$> merchantId
  set
    tbl
    [ GateInfoGeomName =. val name,
      GateInfoGeomPoint =. val point,
      GateInfoGeomDefaultDriverExtra =. val defaultDriverExtra,
      GateInfoGeomAddress =. val address,
      GateInfoGeomMerchantOperatingCityId =. val merchantOperatingCityId',
      GateInfoGeomMerchantId =. val merchantId',
      GateInfoGeomUpdatedAt =. val updatedAt,
      GateInfoGeomCanQueueUpOnGate =. val canQueueUpOnGate,
      GateInfoGeomGeom =. val geom,
      GateInfoGeomGateTags =. val gateTags,
      GateInfoGeomWalkDescription =. val walkDescription
    ]
  where_ $
    tbl ^. GateInfoGeomSpecialLocationId ==. val (toKey $ cast specialLocationId)
      &&. tbl ^. GateInfoGeomName ==. val name
