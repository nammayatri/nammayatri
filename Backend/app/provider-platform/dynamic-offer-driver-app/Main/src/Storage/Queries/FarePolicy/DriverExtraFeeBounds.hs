{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.DriverExtraFeeBounds where

import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy as FarePolicy
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.FarePolicy.DriverExtraFeeBounds
import qualified Storage.Tabular.FarePolicy.DriverExtraFeeBounds as DFP

findAll' ::
  ( Transactionable m,
    MonadThrow m,
    Log m
  ) =>
  Id DFP.FarePolicy ->
  DTypeBuilder m [DriverExtraFeeBoundsT]
findAll' farePolicyId = do
  Esq.findAll' $ do
    driverExtraFeeBounds <- from $ table @DriverExtraFeeBoundsT
    where_ $
      driverExtraFeeBounds ^. DriverExtraFeeBoundsFarePolicyId ==. val (toKey farePolicyId)
    orderBy [asc $ driverExtraFeeBounds ^. DriverExtraFeeBoundsStartDistance]
    return driverExtraFeeBounds

deleteAll' :: Id DFP.FarePolicy -> FullEntitySqlDB ()
deleteAll' farePolicyId =
  Esq.delete' $ do
    driverExtraFeeBounds <- from $ table @DriverExtraFeeBoundsT
    where_ $
      driverExtraFeeBounds ^. DriverExtraFeeBoundsFarePolicyId ==. val (toKey farePolicyId)

create :: DFP.FullDriverExtraFeeBounds -> SqlDB ()
create = Esq.create

findByFarePolicyIdAndStartDistance :: Transactionable m => Id FarePolicy.FarePolicy -> Meters -> m (Maybe DFP.FullDriverExtraFeeBounds)
findByFarePolicyIdAndStartDistance farePolicyId startDistance = Esq.findOne $ do
  farePolicy <- from $ table @DFP.DriverExtraFeeBoundsT
  where_ $
    farePolicy ^. DFP.DriverExtraFeeBoundsFarePolicyId ==. val (toKey farePolicyId)
      &&. farePolicy ^. DFP.DriverExtraFeeBoundsStartDistance ==. val startDistance
  pure farePolicy

update :: Id FarePolicy.FarePolicy -> Meters -> HighPrecMoney -> HighPrecMoney -> SqlDB ()
update farePolicyId startDistace minFee maxFee = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DFP.DriverExtraFeeBoundsMinFee =. val minFee,
        DFP.DriverExtraFeeBoundsMaxFee =. val maxFee
      ]
    where_ $
      tbl ^. DFP.DriverExtraFeeBoundsFarePolicyId ==. val (toKey farePolicyId)
        &&. tbl ^. DFP.DriverExtraFeeBoundsStartDistance ==. val startDistace
