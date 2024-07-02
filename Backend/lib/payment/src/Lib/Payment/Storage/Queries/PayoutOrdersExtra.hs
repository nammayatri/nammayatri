{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PayoutOrdersExtra where

import Data.Time (UTCTime (UTCTime, utctDay), secondsToDiffTime)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import Lib.Payment.Domain.Types.PayoutOrder (PayoutOrder)
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutOrder as BeamPO
import qualified Sequelize as Se

updateLastCheckedOn :: (BeamFlow m r) => [Text] -> m ()
updateLastCheckedOn payoutOrderIds = do
  now <- getCurrentTime
  let lastCheckedAt = UTCTime (utctDay now) (secondsToDiffTime 0)
  updateWithKV
    [ Se.Set BeamPO.lastStatusCheckedAt (Just lastCheckedAt),
      Se.Set BeamPO.updatedAt now
    ]
    [Se.Is BeamPO.orderId (Se.In payoutOrderIds)]
