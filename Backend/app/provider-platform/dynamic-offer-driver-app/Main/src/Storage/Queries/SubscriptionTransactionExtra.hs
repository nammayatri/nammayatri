{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SubscriptionTransactionExtra where

import Data.Maybe
import Domain.Types.Person
import qualified Domain.Types.SubscriptionTransaction as Domain
import EulerHS.Prelude
import Kernel.Beam.Functions
import Kernel.External.Payment.Juspay.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.SubscriptionTransaction as Beam
import Storage.Queries.OrphanInstances.SubscriptionTransaction ()

findAllByDriverDateAmountAndStatus ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  UTCTime ->
  UTCTime ->
  Maybe TransactionStatus ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Int ->
  Int ->
  m [Domain.SubscriptionTransaction]
findAllByDriverDateAmountAndStatus (Id driverId) fromDate toDate mbStatus mbMinAmount mbMaxAmount limit offset = do
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Is Beam.driverId $ Se.Eq driverId,
            Se.Is Beam.createdAt $ Se.GreaterThanOrEq fromDate,
            Se.Is Beam.createdAt $ Se.LessThanOrEq toDate
          ]
            <> ([Se.Is Beam.status $ Se.Eq (fromJust mbStatus) | isJust mbStatus])
            <> ([Se.Is Beam.amount $ Se.GreaterThanOrEq (fromJust mbMinAmount) | isJust mbMinAmount])
            <> ([Se.Is Beam.amount $ Se.LessThanOrEq (fromJust mbMaxAmount) | isJust mbMaxAmount])
        )
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)
