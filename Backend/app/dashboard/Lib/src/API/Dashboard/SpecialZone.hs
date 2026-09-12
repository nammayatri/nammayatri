{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Special-zone administration, forwarded to the special-zone service.
--
-- provider-dashboard authorised these with @ApiAuth 'SPECIAL_ZONE 'SPECIAL_ZONES
-- 'SPECIAL_ZONE_*@, whose action types are promoted enum constructors and cannot
-- be linked outside lib-dashboard-api. The same decision is made here against
-- the capability table instead: the session is checked by 'DashboardAuth', then
-- 'Verify.verifyAccessLevel' is given the endpoint id -- which for these nullary
-- constructors is the bare constructor name, exactly the key the old access
-- matrix used.
--
-- Each mutating call still writes its audit row, with the request body, as
-- before.
module API.Dashboard.SpecialZone
  ( API,
    handler,
  )
where

import Data.Aeson (Value)
import qualified Domain.Types.DashboardActionType as DashAuth
import qualified Domain.Types.ServerName as DSN
import qualified Domain.Types.Transaction as DT
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.App (MandatoryQueryParam)
import Kernel.Types.Flow (FlowR)
import Kernel.Utils.Common (FlowHandlerR, FlowServerR, MonadFlow, encodeToText, generateGUID, getCurrentTime)
import Servant
import qualified SharedLogic.Transaction as STransaction
import qualified Tools.Auth.ApiAuth as Auth
import Tools.Auth.Dashboard
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow, withDashboardDbFlowHandlerAPI)
import qualified Tools.Auth.Verify as Verify
import qualified Tools.SpecialZoneClient as Client

type API =
  "specialZone"
    :> ( DashboardAuth 'DASHBOARD_USER
           :> "lookup"
           :> MandatoryQueryParam "minLatLng" LatLong
           :> MandatoryQueryParam "maxLatLng" LatLong
           :> Get '[JSON] [Value]
           :<|> DashboardAuth 'DASHBOARD_USER
             :> "create"
             :> ReqBody '[JSON] Value
             :> Post '[JSON] APISuccess
           :<|> DashboardAuth 'DASHBOARD_USER
             :> "update"
             :> ReqBody '[JSON] Value
             :> Post '[JSON] APISuccess
           :<|> DashboardAuth 'DASHBOARD_USER
             :> "delete"
             :> MandatoryQueryParam "id" Text
             :> Delete '[JSON] APISuccess
       )

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
handler = lookupSpecialZone :<|> createSpecialZone :<|> updateSpecialZone :<|> deleteSpecialZone

-- | Audit row for a special-zone mutation: SPECIAL_ZONE server, no merchant,
-- request body. The action stays typed; it is rendered only on the way to the
-- @transaction.endpoint@ column.
auditEntry :: MonadFlow m => TokenInfo -> DashAuth.DashboardActionType -> Maybe Value -> m (DT.Transaction DashAuth.DashboardActionType)
auditEntry tokenInfo action request = do
  uid <- generateGUID
  now <- getCurrentTime
  pure
    DT.Transaction
      { id = uid,
        requestorId = Just tokenInfo.personId,
        merchantId = Nothing,
        serverName = Just DSN.SPECIAL_ZONE,
        endpoint = DT.ActionAPI action,
        commonDriverId = Nothing,
        commonRideId = Nothing,
        request = encodeToText <$> request,
        response = Nothing,
        responseError = Nothing,
        createdAt = now
      }

lookupSpecialZone :: DashboardLoginFlow (FlowR r) r => TokenInfo -> LatLong -> LatLong -> FlowHandlerR r [Value]
lookupSpecialZone tokenInfo minLatLng maxLatLng = withDashboardDbFlowHandlerAPI $ do
  void $ Verify.verifyAccessLevel (Auth.showUserActionType DashAuth.SPECIAL_ZONE_LOOKUP) tokenInfo.personId
  Client.callSpecialZone (.lookupSpecialZone) minLatLng maxLatLng

createSpecialZone :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Value -> FlowHandlerR r APISuccess
createSpecialZone tokenInfo req = withDashboardDbFlowHandlerAPI $ do
  void $ Verify.verifyAccessLevel (Auth.showUserActionType DashAuth.SPECIAL_ZONE_CREATE) tokenInfo.personId
  auditEntry tokenInfo DashAuth.SPECIAL_ZONE_CREATE (Just req) >>= \txn ->
    STransaction.withTransactionStoring txn $
      Client.callSpecialZone (.createSpecialZone) req

updateSpecialZone :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Value -> FlowHandlerR r APISuccess
updateSpecialZone tokenInfo req = withDashboardDbFlowHandlerAPI $ do
  void $ Verify.verifyAccessLevel (Auth.showUserActionType DashAuth.SPECIAL_ZONE_UPDATE) tokenInfo.personId
  auditEntry tokenInfo DashAuth.SPECIAL_ZONE_UPDATE (Just req) >>= \txn ->
    STransaction.withTransactionStoring txn $
      Client.callSpecialZone (.updateSpecialZone) req

deleteSpecialZone :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Text -> FlowHandlerR r APISuccess
deleteSpecialZone tokenInfo zoneId = withDashboardDbFlowHandlerAPI $ do
  void $ Verify.verifyAccessLevel (Auth.showUserActionType DashAuth.SPECIAL_ZONE_DELETE) tokenInfo.personId
  auditEntry tokenInfo DashAuth.SPECIAL_ZONE_DELETE Nothing >>= \txn ->
    STransaction.withTransactionStoring txn $
      Client.callSpecialZone (.deleteSpecialZone) zoneId
