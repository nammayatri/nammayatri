{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Exotel
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.Common.Exotel as Common
import Data.List (nub, sort)
import qualified Domain.Types.ServerName as DSN
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Utils.Common (MonadFlow, decodeFromText, fork, logTagInfo, throwError, withFlowHandlerAPI')
import qualified ProviderPlatformClient.DynamicOfferDriver.Exotel as Client
import qualified RiderPlatformClient.RiderApp as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import qualified Storage.Queries.Transaction as QT

type API =
  "exotel"
    :> Capture "exotelToken" Text
    :> Common.ExotelHeartbeatAPI

handler :: FlowServer API
handler =
  exotelHeartbeat

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.ExotelEndpoint ->
  DSN.ServerName ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint serverName =
  T.buildTransaction (DT.ExotelAPI endpoint) (Just serverName) Nothing Nothing Nothing

-- store request and call bap/bpp only when status changed OK to not OK and vice versa, or affected numbers changed.
exotelHeartbeat :: Text -> Common.ExotelHeartbeatReq -> FlowHandler APISuccess
exotelHeartbeat incomingExotelToken req = withFlowHandlerAPI' $ do
  exotelToken <- asks (.exotelToken)
  unless (incomingExotelToken == exotelToken) $
    throwError $ InvalidToken incomingExotelToken
  let serverNames = [DSN.APP_BACKEND_MANAGEMENT, DSN.DRIVER_OFFER_BPP_MANAGEMENT]
  needToCallApps <- forM serverNames $ \serverName -> do
    mbLastTransaction <- Esq.runInReplica $ QT.fetchLastTransaction (DT.ExotelAPI Common.ExotelHeartbeatEndpoint) serverName
    let mbLastReq =
          mbLastTransaction
            >>= (.request)
            >>= decodeFromText @(Common.ReqWithoutSecrets Common.ExotelHeartbeatReq)
    let mbLastStatus = mbLastReq <&> (.statusType)
    let lastTransactionFailed =
          mbLastTransaction
            >>= (.responseError)
            & isJust
    let lastStatusWasNotOk = mbLastStatus /= Just Common.OK || lastTransactionFailed
        lastStatusWasOk = not lastStatusWasNotOk
        affectedPhonesChanged = Just (getAffectedPhoneNumberSids req) /= (getAffectedPhoneNumberSids <$> mbLastReq)
        needToCallApp = if req.statusType /= Common.OK then lastStatusWasOk || affectedPhonesChanged else lastStatusWasNotOk
    when needToCallApp $ do
      fork ("exotelHeartbeat:" <> show serverName) $ do
        transaction <- buildTransaction Common.ExotelHeartbeatEndpoint serverName (Just req)
        T.withTransactionStoring transaction $
          void $ callExotelHeartbeat serverName
    pure needToCallApp

  when (or needToCallApps) $
    logTagInfo "exotelHeartbeat: " $ show req.statusType

  pure Success
  where
    callExotelHeartbeat DSN.APP_BACKEND_MANAGEMENT = Client.callRiderAppExotelApi (.exotelHeartbeat) req
    callExotelHeartbeat DSN.DRIVER_OFFER_BPP_MANAGEMENT = Client.callDynamicOfferDriverAppExotelApi (.exotelHeartbeat) req
    callExotelHeartbeat _ = throwError $ InternalError "Exotel is not configured with Special Zone server"

    getAffectedPhoneNumberSids req' = nub . sort . (<&> (.phoneNumberSid)) $ req'.incomingAffected <> req'.outgoingAffected
