{-# LANGUAGE OverloadedLabels #-}

module ExternalAPI.Flow where

import App.Types
import Beckn.Types.Core.API.Call as API
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Error
import Beckn.Types.Id
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Callback (WithBecknCallback, withBecknCallback)
import qualified Beckn.Utils.Error.BecknAPIError as Beckn
import Control.Arrow ((>>>))
import Control.Lens.Operators ((?~))
import qualified Data.Text as T
import EulerHS.Prelude
import Storage.Queries.Case as Case
import Storage.Queries.Organization as Org
import Types.Error
import Utils.Auth
import Utils.Common

getGatewayUrl :: Flow BaseUrl
getGatewayUrl = do
  appConfig <- ask
  gatewayShortId <- appConfig.xGatewaySelector & fromMaybeM GatewaySelectorNotSet
  gatewayOrg <- Org.findOrgByShortId $ ShortId gatewayShortId
  case gatewayShortId of
    "NSDL.BG.1" -> appConfig.xGatewayNsdlUrl & fromMaybeM NSDLBaseUrlNotSet
    "JUSPAY.BG.1" -> gatewayOrg.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
    _ -> throwError UnsupportedGatewaySelector

withCallback ::
  Org.Organization ->
  WithBecknCallback api callback_success AppEnv
withCallback transporter action api context cbUrl f = do
  let bppShortId = getShortId $ transporter.shortId
  authKey <- getHttpManagerKey bppShortId
  bppUri <- makeBppUrl (transporter.id)
  let context' = context & #bpp_uri ?~ bppUri
  withBecknCallback (Just authKey) action api context' cbUrl f

callBAP ::
  Beckn.IsBecknAPI api (CallbackReq req) =>
  Text ->
  Proxy api ->
  Org.Organization ->
  Id Case ->
  Either Error req ->
  Flow ()
callBAP action api transporter caseId contents = do
  case_ <- Case.findById caseId >>= fromMaybeM CaseNotFound
  bapCallbackUrl <-
    (case_.udf4) & fromMaybeM (CaseFieldNotPresent "udf4")
      >>= (Id >>> Org.findOrganizationById)
      >>= ((^. #callbackUrl) >>> fromMaybeM (OrgFieldNotPresent "callback_url"))
  let bppShortId = getShortId $ transporter.shortId
  authKey <- getHttpManagerKey bppShortId
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

makeBppUrl :: Id Org.Organization -> Flow BaseUrl
makeBppUrl (Id transporterId) =
  asks nwAddress
    <&> #baseUrlPath %~ (<> "/" <> T.unpack transporterId)

initiateCall :: CallReq -> Flow ()
initiateCall req = do
  url <- xAppUri <$> ask
  Beckn.callBecknAPI Nothing (Just "UNABLE_TO_CALL") "call/to_customer" API.callsAPI url req
