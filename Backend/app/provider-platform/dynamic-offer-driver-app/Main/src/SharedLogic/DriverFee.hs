{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverFee where

import Domain.Types.DriverFee
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBFlow, runTransaction)
import Kernel.Types.Common (MonadFlow, generateGUID)
import Kernel.Utils.Common (generateShortId)
import Storage.Queries.DriverFee (create, updateStatus)

mergeDriverFee :: (MonadFlow m, EsqDBFlow m r) => DriverFee -> DriverFee -> UTCTime -> m ()
mergeDriverFee oldFee newFee now = do
  id <- generateGUID
  shortId <- generateShortId
  let driverId = newFee.driverId
      govtCharges = newFee.govtCharges + oldFee.govtCharges
      platformFee = PlatformFee (oldFee.platformFee.fee + newFee.platformFee.fee) (oldFee.platformFee.cgst + newFee.platformFee.cgst) (oldFee.platformFee.sgst + newFee.platformFee.sgst)
      numRides = oldFee.numRides + newFee.numRides
      payBy = newFee.endTime
      totalEarnings = oldFee.totalEarnings + newFee.totalEarnings
      startTime = oldFee.startTime
      endTime = newFee.endTime
      status = PAYMENT_OVERDUE
      createdAt = now
      updatedAt = now
  let newDriverFee = DriverFee {..}
  runTransaction $ do
    updateStatus INACTIVE oldFee.id now
    updateStatus INACTIVE newFee.id now
    create newDriverFee
