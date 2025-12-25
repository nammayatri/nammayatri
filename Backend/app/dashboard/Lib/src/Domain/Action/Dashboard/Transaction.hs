{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Transaction where

import Dashboard.Common
import qualified "dashboard-helper-api" Dashboard.Common.Driver as Common
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Transaction as DT
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Beam.BeamFlow (BeamFlow)
import qualified Storage.Queries.Transaction as QT
import Tools.Auth

data ListTransactionRes = ListTransactionRes
  { list :: [DT.TransactionAPIEntity],
    summary :: Summary
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

listTransactions :: (BeamFlow m r, EncFlow m r) => TokenInfo -> Maybe Text -> Maybe Integer -> Maybe Integer -> Maybe (Id DP.Person) -> Maybe (Id Common.Driver) -> Maybe (Id Common.Ride) -> Maybe DT.Endpoint -> m ListTransactionRes
listTransactions _ mbSearchString mbLimit mbOffset mbRequestorId mbDriverId mbRideId mbEndpoint = do
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString
  transactionList <- B.runInReplica $ QT.findAllTransactionsByLimitOffset mbSearchString mbSearchStrDBHash mbLimit mbOffset mbRequestorId mbDriverId mbRideId mbEndpoint
  transactions <- forM transactionList $ \(transaction, encPerson) -> do
    decPerson <- decrypt encPerson
    pure $ mkTransactionAPIEntity transaction decPerson
  let count = length transactions
  let summary = Summary {totalCount = 10000, count}
  pure $ ListTransactionRes {list = transactions, summary = summary}
  where
    mkTransactionAPIEntity DT.Transaction {..} requestor = do
      DT.TransactionAPIEntity {requestor = mkRequestorAPIEntity requestor, ..}
    mkRequestorAPIEntity DP.Person {..} = DT.RequestorAPIEntity {registeredAt = createdAt, ..}
