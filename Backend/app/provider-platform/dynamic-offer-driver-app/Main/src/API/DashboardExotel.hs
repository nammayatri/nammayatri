{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Exotel heartbeat: one request that fans out to BOTH application servers.
--
-- This was the last thing keeping provider-dashboard on the request path, and
-- the fan-out was the reason -- whichever server hosts it has to reach the
-- other. That is not the obstacle it looked like: the two servers already call
-- each other constantly, and hosting it here turns one of the two hops into a
-- local call.
--
-- Authenticated by the shared Exotel token, not an operator session.
--
-- Suppression logic is unchanged: an application server is called only when the
-- status flips OK <-> not-OK, or the affected numbers change. That decision reads
-- the last audit row for this endpoint, which lives in the dashboard database --
-- hence 'runInDashboardDb' around the read and around the row each call writes.
module API.DashboardExotel
  ( API,
    handler,
  )
where

import qualified "lib-dashboard" Dashboard.Common.Exotel as Common
import Data.List (nub, sort)
import qualified Domain.Action.Dashboard.Exotel as DExotel
import qualified "lib-dashboard" Domain.Types.ServerName as DSN
import Environment
import Kernel.Beam.Functions (runInDashboardDb)
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified "lib-dashboard" Storage.Beam.SchemaInstances ()
import qualified "lib-dashboard" Storage.Queries.AuditTransaction as QAudit
import qualified "lib-dashboard" Storage.Queries.TransactionView as QT
import qualified "lib-dashboard" Tools.ExotelClient as ExotelClient

type API =
  "exotel"
    :> Capture "exotelToken" Text
    :> Common.ExotelHeartbeatAPI

handler :: FlowServer API
handler = exotelHeartbeat

-- | The text @Domain.Types.Transaction.Endpoint@'s Show produces for this
-- action, and therefore the key its audit rows are stored under. It must stay in
-- step with that instance: provider-dashboard still writes rows with it.
exotelHeartbeatEndpoint :: Text
exotelHeartbeatEndpoint = "ExotelAPI ExotelHeartbeatEndpoint"

exotelHeartbeat :: Text -> Common.ExotelHeartbeatReq -> FlowHandler APISuccess
exotelHeartbeat incomingExotelToken req = withFlowHandlerAPI' $ do
  exotelToken <- asks (.exotelToken)
  unless (incomingExotelToken == exotelToken) $
    throwError $ InvalidToken incomingExotelToken
  let serverNames = [DSN.APP_BACKEND_MANAGEMENT, DSN.DRIVER_OFFER_BPP_MANAGEMENT]
  needToCallApps <- forM serverNames $ \serverName -> do
    mbLastTransaction <- runInDashboardDb $ B.runInReplica $ QT.fetchLastTransaction exotelHeartbeatEndpoint serverName
    let mbLastReq =
          mbLastTransaction
            >>= (.request)
            >>= decodeFromText @(Common.ReqWithoutSecrets Common.ExotelHeartbeatReq)
    let mbLastStatus = mbLastReq <&> (.statusType)
    let lastTransactionFailed = mbLastTransaction >>= (.responseError) & isJust
    let lastStatusWasNotOk = mbLastStatus /= Just Common.OK || lastTransactionFailed
        lastStatusWasOk = not lastStatusWasNotOk
        affectedPhonesChanged = Just (getAffectedPhoneNumberSids req) /= (getAffectedPhoneNumberSids <$> mbLastReq)
        needToCallApp = if req.statusType /= Common.OK then lastStatusWasOk || affectedPhonesChanged else lastStatusWasNotOk
    when needToCallApp $
      fork ("exotelHeartbeat:" <> show serverName) $
        runInDashboardDb $
          QAudit.withAuditTransactionStoring (auditEntry serverName) $
            void $
              callExotelHeartbeat serverName
    pure needToCallApp

  when (or needToCallApps) $
    logTagInfo "exotelHeartbeat: " $ show req.statusType

  pure Success
  where
    auditEntry serverName =
      QAudit.AuditTransaction
        { requestorId = Nothing,
          merchantId = Nothing,
          serverName = Just serverName,
          endpoint = exotelHeartbeatEndpoint,
          commonDriverId = Nothing,
          commonRideId = Nothing,
          request = Just (encodeToText (Common.hideSecrets req)),
          response = Nothing,
          responseError = Nothing
        }

    -- This server IS DRIVER_OFFER_BPP_MANAGEMENT, so that half of the fan-out is
    -- a direct call; the rider-app half stays an HTTP hop.
    callExotelHeartbeat DSN.DRIVER_OFFER_BPP_MANAGEMENT = DExotel.exotelHeartbeat req
    callExotelHeartbeat DSN.APP_BACKEND_MANAGEMENT = ExotelClient.callExotelHeartbeat DSN.APP_BACKEND_MANAGEMENT (.exotelHeartbeat) req
    callExotelHeartbeat _ = throwError $ InternalError "Exotel is not configured with Special Zone server"

    getAffectedPhoneNumberSids req' = nub . sort . (<&> (.phoneNumberSid)) $ req'.incomingAffected <> req'.outgoingAffected
