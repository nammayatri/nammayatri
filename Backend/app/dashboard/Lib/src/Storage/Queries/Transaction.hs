{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Writes to the dashboard audit trail, parameterised by the action type of
-- whichever package owns the endpoint.
--
-- The @endpoint@ column is the one place the typed action is rendered: it has
-- always stored @show@ output, so the conversion lives here and nowhere else.
-- Rows are written through 'Storage.Queries.AuditTransaction', the table's only
-- Beam writer, and read back as text by 'Storage.Queries.TransactionView'.
module Storage.Queries.Transaction
  ( create,
    createDashboardTransaction,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.DashboardActionType as DashAuth
import qualified Domain.Types.Transaction as DT
import Kernel.Prelude
import Kernel.Types.Id
import Storage.Beam.BeamFlow
import qualified Storage.Queries.AuditTransaction as QAudit

toAuditTransaction :: Show uat => DT.Transaction uat -> QAudit.AuditTransaction
toAuditTransaction txn =
  QAudit.AuditTransaction
    { requestorId = getId <$> txn.requestorId,
      merchantId = getId <$> txn.merchantId,
      serverName = txn.serverName,
      endpoint = T.pack $ show txn.endpoint,
      commonDriverId = getId <$> txn.commonDriverId,
      commonRideId = getId <$> txn.commonRideId,
      request = txn.request,
      response = txn.response,
      responseError = txn.responseError
    }

create :: (BeamFlow m r, Show uat) => DT.Transaction uat -> m ()
create txn = QAudit.writeAuditTransactionWithId txn.id.getId txn.createdAt (toAuditTransaction txn)

-- | lib-dashboard's own audit rows (login, user administration).
createDashboardTransaction :: BeamFlow m r => DT.Transaction DashAuth.DashboardActionType -> m ()
createDashboardTransaction = create
