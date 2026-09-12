{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The operator-action audit trail listing, servable by an application server.
-- See 'Domain.Types.TransactionView'.
module Domain.Action.Dashboard.TransactionView
  ( listTransactions,
  )
where

import Dashboard.Common (Summary (..))
import Data.Time (addUTCTime)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransactionView as DT
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Beam.BeamFlow (BeamFlow)
import qualified Storage.Queries.TransactionView as QT
import Tools.Auth.Dashboard

listTransactions ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe (Id DP.Person) ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m DT.ListTransactionRes
listTransactions _ mbSearchString mbLimit mbOffset mbRequestorId mbDriverId mbRideId mbEndpoint mbFrom mbTo = do
  -- Unbounded scans of the audit table are expensive, so an absent range means
  -- the last 7 days rather than everything.
  (defaultedFrom, defaultedTo) <- case (mbFrom, mbTo) of
    (Nothing, Nothing) -> do
      now <- getCurrentTime
      pure (Just (addUTCTime (negate (7 * 24 * 60 * 60) :: NominalDiffTime) now), Just now)
    _ -> pure (mbFrom, mbTo)
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString
  rows <-
    B.runInReplica $
      QT.findAllTransactionsByLimitOffset mbSearchString mbSearchStrDBHash mbLimit mbOffset mbRequestorId mbDriverId mbRideId mbEndpoint defaultedFrom defaultedTo
  transactions <- forM rows $ \(row, encPerson) -> do
    decPerson <- decrypt encPerson
    pure $ DT.mkTransactionAPIEntity row decPerson
  -- totalCount is a fixed sentinel, matching lib-dashboard-api: the listing does
  -- no count query, and the frontend is documented not to treat it as a total.
  pure $ DT.ListTransactionRes {list = transactions, summary = Summary {totalCount = 10000, count = length transactions}}
