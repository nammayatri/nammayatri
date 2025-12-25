{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Queries.SpecialLocationGeom where

import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Types.Id
import Lib.Tabular.SpecialLocationGeom
import qualified Lib.Types.SpecialLocation as D

create :: D.SpecialLocation -> SqlDB ()
create = Esq.create

updateSpecialLocation :: D.SpecialLocation -> SqlDB ()
updateSpecialLocation D.SpecialLocation {..} = Esq.update $ \tbl -> do
  let merchantOperatingCityId' = Kernel.Types.Id.getId <$> merchantOperatingCityId
      merchantId' = Kernel.Types.Id.getId <$> merchantId
  set
    tbl
    [ SpecialLocationGeomLocationName =. val locationName,
      SpecialLocationGeomCategory =. val category,
      SpecialLocationGeomGeom =. val geom,
      SpecialLocationGeomMerchantOperatingCityId =. val merchantOperatingCityId',
      SpecialLocationGeomMerchantId =. val merchantId',
      SpecialLocationGeomUpdatedAt =. val updatedAt
    ]
  where_ $
    tbl ^. SpecialLocationGeomId ==. val id.getId
