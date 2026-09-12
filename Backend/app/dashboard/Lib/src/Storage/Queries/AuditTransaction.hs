{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Writing the dashboard audit trail from an application server.
--
-- The existing writer ('Storage.Queries.Transaction', via
-- 'SharedLogic.Transaction') goes through @Domain.Types.Transaction@, whose
-- @Endpoint@ names the dashboard API action types and therefore lives in
-- lib-dashboard-api -- which an application server cannot depend on.
--
-- This writes the same @transaction@ row with the endpoint as the plain
-- MODULE\/RESOURCE\/ACTION text already stored in that column, so both writers
-- produce identical rows and the trail stays one table with one shape.
module Storage.Queries.AuditTransaction
  ( AuditTransaction (..),
    writeAuditTransaction,
    withAuditTransactionStoring,
    dashboardUserLogin,
    dashboardUserLogout,
    dashboardTwoFactorAdminReset,
    dashboardUserDelete,
    dashboardUserPasswordResetByAdmin,
    dashboardUserEmailChangeByAdmin,
    dashboardUserMobileChangeByAdmin,
    dashboardUserRoleAssign,
  )
where

import qualified Domain.Types.ServerName as DSN
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common (encodeToText, generateGUID, getCurrentTime, throwError)
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Transaction as BeamT
import qualified Tools.Error as E

-- | One audited action, as the caller describes it. The row's identity and
-- timestamp are assigned by 'writeAuditTransaction'.
data AuditTransaction = AuditTransaction
  { requestorId :: Maybe Text,
    merchantId :: Maybe Text,
    serverName :: Maybe DSN.ServerName,
    -- | Endpoint id in the same form capability_endpoint uses:
    -- MODULE/RESOURCE/ACTION.
    endpoint :: Text,
    commonDriverId :: Maybe Text,
    commonRideId :: Maybe Text,
    request :: Maybe Text,
    response :: Maybe Text,
    responseError :: Maybe Text
  }

-- | An 'AuditTransaction' with the identity and timestamp filled in. Separate
-- so callers cannot supply either, and so the ToTType' instance has every
-- column it needs.
data StoredAuditTransaction = StoredAuditTransaction
  { txnId :: Text,
    txnCreatedAt :: UTCTime,
    entry :: AuditTransaction
  }

instance ToTType' BeamT.Transaction StoredAuditTransaction where
  toTType' StoredAuditTransaction {txnId, txnCreatedAt, entry = AuditTransaction {..}} =
    BeamT.TransactionT
      { id = txnId,
        createdAt = txnCreatedAt,
        requestorId = requestorId,
        merchantId = merchantId,
        serverName = serverName,
        endpoint = endpoint,
        commonDriverId = commonDriverId,
        commonRideId = commonRideId,
        request = request,
        response = response,
        responseError = responseError
      }

-- | Insert one audit row.
--
-- On an application server this must be wrapped in @runInDashboardDb@ -- and
-- only this, not the surrounding handler, whose own queries belong to the
-- application database.
writeAuditTransaction :: BeamFlow m r => AuditTransaction -> m ()
writeAuditTransaction entry = do
  txnId <- generateGUID
  txnCreatedAt <- getCurrentTime
  createWithKV @BeamT.TransactionT StoredAuditTransaction {txnId, txnCreatedAt, entry}

-- | Run an outward call and record how it went, as one audit row.
--
-- Mirrors @SharedLogic.Transaction.withResponseTransactionStoring@ in
-- lib-dashboard-api: the row is written once, after the call, carrying either
-- the response or the error. Only the endpoint's type differs -- text here
-- rather than the app-typed @Endpoint@ enum -- so the row shape is identical.
withAuditTransactionStoring ::
  ( BeamFlow m r,
    MonadCatch m,
    ToJSON response
  ) =>
  AuditTransaction ->
  m response ->
  m response
withAuditTransactionStoring entry call = handle errorHandler $ do
  response <- call
  writeAuditTransaction entry {response = Just (encodeToText response)}
  pure response
  where
    errorHandler (err :: E.Error) = do
      writeAuditTransaction entry {responseError = Just (show err)}
      throwError err

-- Endpoint names for the login/user-administration actions.
--
-- These are the exact strings @Domain.Types.Transaction.Endpoint@'s Show
-- instance produces, which is what its Beam instance writes to the column. The
-- two writers must agree: the audit trail is one table read by endpoint name,
-- and provider-dashboard still writes the other side of it.

dashboardUserLogin :: Text
dashboardUserLogin = "DASHBOARD_USER/LOGIN"

dashboardUserLogout :: Text
dashboardUserLogout = "DASHBOARD_USER/LOGOUT"

dashboardTwoFactorAdminReset :: Text
dashboardTwoFactorAdminReset = "DASHBOARD_USER/TWO_FACTOR_ADMIN_RESET"

dashboardUserDelete :: Text
dashboardUserDelete = "DASHBOARD_USER/DELETE"

dashboardUserPasswordResetByAdmin :: Text
dashboardUserPasswordResetByAdmin = "DASHBOARD_USER/PASSWORD_RESET_BY_ADMIN"

dashboardUserEmailChangeByAdmin :: Text
dashboardUserEmailChangeByAdmin = "DASHBOARD_USER/EMAIL_CHANGE_BY_ADMIN"

dashboardUserMobileChangeByAdmin :: Text
dashboardUserMobileChangeByAdmin = "DASHBOARD_USER/MOBILE_CHANGE_BY_ADMIN"

dashboardUserRoleAssign :: Text
dashboardUserRoleAssign = "DASHBOARD_USER/ROLE_ASSIGN"
