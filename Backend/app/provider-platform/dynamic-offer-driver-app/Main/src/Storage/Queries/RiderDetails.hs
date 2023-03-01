{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.RiderDetails where

import Domain.Types.RiderDetails
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.RiderDetails

create :: RiderDetails -> SqlDB m ()
create = Esq.create

findById ::
  forall m ma.
  Transactionable ma m =>
  Proxy ma ->
  Id RiderDetails ->
  m (Maybe RiderDetails)
findById _ = Esq.findById @m @ma

findByMobileNumber ::
  forall m ma r.
  (MonadThrow m, Log m, Transactionable ma m, EncFlow m r) =>
  Text ->
  Proxy ma ->
  m (Maybe RiderDetails)
findByMobileNumber mobileNumber_ _ = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  Esq.findOne @m @ma $ do
    riderDetails <- from $ table @RiderDetailsT
    where_ $ riderDetails ^. RiderDetailsMobileNumberHash ==. val mobileNumberDbHash
    return riderDetails
