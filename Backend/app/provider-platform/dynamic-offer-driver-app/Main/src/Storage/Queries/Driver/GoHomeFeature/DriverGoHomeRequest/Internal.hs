{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest.Internal where

import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.Person (Driver)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common hiding (Value)
import Storage.Tabular.Driver.GoHomeFeature.DriverGoHomeRequest

getDriverGoHomeReqNearby ::
  (Transactionable m, MonadTime m) =>
  [Id Driver] ->
  m [DriverGoHomeRequest]
getDriverGoHomeReqNearby driverIds = do
  Esq.findAll $ do
    goHomeReq <- from $ table @DriverGoHomeRequestT
    where_ $
      goHomeReq ^. DriverGoHomeRequestDriverId `in_` valList (toKey <$> driverIds)
        &&. goHomeReq ^. DriverGoHomeRequestStatus ==. val DDGR.ACTIVE
    return goHomeReq
