{-# LANGUAGE OverloadedLabels #-}

module ExternalAPI.Flow where

import Beckn.Types.Core.API.Call as API
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Error
import Beckn.Types.Id
import Beckn.Utils.Callback (WithBecknCallback, withBecknCallback)
import qualified Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Control.Arrow ((>>>))
import Control.Lens.Operators ((?~))
import qualified Data.Text as T
import EulerHS.Prelude
import Storage.Queries.Case as Case
import Storage.Queries.Organization as Org
import Types.Error
import Types.Metrics (CoreMetrics)
import Types.Storage.Case as Case
import Types.Storage.Organization as Org
import Utils.Auth
import Utils.Common

getGatewayUrl ::
  ( DBFlow m r,
    HasFlowEnv m r ["xGatewaySelector" ::: Maybe Text, "xGatewayNsdlUrl" ::: Maybe BaseUrl]
  ) =>
  m BaseUrl
getGatewayUrl = do
  appConfig <- ask
  gatewayShortId <- appConfig.xGatewaySelector & fromMaybeM GatewaySelectorNotSet
  gatewayOrg <- Org.findOrgByShortId (ShortId gatewayShortId) >>= fromMaybeM OrgNotFound
  case gatewayShortId of
    "NSDL.BG.1" -> appConfig.xGatewayNsdlUrl & fromMaybeM NSDLBaseUrlNotSet
    "JUSPAY.BG.1" -> gatewayOrg.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
    _ -> throwError UnsupportedGatewaySelector

withCallback ::
  HasFlowEnv m r '["nwAddress" ::: BaseUrl] =>
  Org.Organization ->
  WithBecknCallback api callback_success m
withCallback = withCallback' identity

withCallback' ::
  (m () -> m ()) ->
  HasFlowEnv m r '["nwAddress" ::: BaseUrl] =>
  Org.Organization ->
  WithBecknCallback api callback_success m
withCallback' doWithCallback transporter action api context cbUrl f = do
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
  bppUri <- makeBppUrl (transporter.id)
  let context' = context & #bpp_uri ?~ bppUri
  withBecknCallback doWithCallback (Just authKey) action api context' cbUrl f

callBAP ::
  ( DBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Beckn.IsBecknAPI api (CallbackReq req) =>
  Text ->
  Proxy api ->
  Org.Organization ->
  Id Case ->
  Either Error req ->
  m ()
callBAP action api transporter caseId contents = do
  case_ <- Case.findById caseId >>= fromMaybeM CaseNotFound
  bapCallbackUrl <-
    (case_.udf4) & fromMaybeM (CaseFieldNotPresent "udf4")
      >>= (Id >>> Org.findOrganizationById)
      >>= fromMaybeM OrgNotFound
      >>= ((.callbackUrl) >>> fromMaybeM (OrgFieldNotPresent "callback_url"))
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
  txnId <-
    (getShortId case_.shortId)
      & T.split (== '_')
      & reverse
      & listToMaybe
      & fromMaybeM (InternalError "Cannot exctract transaction id from case.short_id")
  bppUri <- makeBppUrl (transporter.id)
  context <- buildContext action txnId (Just bapCallbackUrl) (Just bppUri)
  Beckn.callBecknAPI (Just authKey) Nothing action api bapCallbackUrl $
    CallbackReq {contents, context}

makeBppUrl ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Id Org.Organization ->
  m BaseUrl
makeBppUrl (Id transporterId) =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack transporterId)

initiateCall ::
  ( HasFlowEnv m r '["xAppUri" ::: BaseUrl],
    CoreMetrics m
  ) =>
  CallReq ->
  m ()
initiateCall req = do
  url <- asks (.xAppUri)
  Beckn.callBecknAPI Nothing (Just "UNABLE_TO_CALL") "call/to_customer" API.callsAPI url req
