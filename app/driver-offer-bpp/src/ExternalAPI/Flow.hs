{-# LANGUAGE OverloadedLabels #-}

module ExternalAPI.Flow where

import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as API
import qualified Beckn.Types.Core.Taxi.API.OnSelect as API
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as API
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import qualified Beckn.Types.Core.Taxi.OnSelect as OnSelect
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import Beckn.Types.Id
import Beckn.Utils.Callback (WithBecknCallbackMig, withBecknCallbackMig)
import qualified Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Beckn.Utils.Servant.SignatureAuth
import Control.Lens.Operators ((?~))
import qualified Data.Text as T
import qualified Domain.Types.Booking as DRB
import Domain.Types.Organization as Org
import qualified Domain.Types.SearchRequest as DSR
import EulerHS.Prelude
import Tools.Metrics (CoreMetrics)
import Utils.Common

withCallback ::
  HasFlowEnv m r '["nwAddress" ::: BaseUrl, "httpClientOptions" ::: HttpClientOptions] =>
  Org.Organization ->
  WithBecknCallbackMig api callback_success m
withCallback = withCallback' withRetry

withCallback' ::
  (m () -> m ()) ->
  HasFlowEnv m r '["nwAddress" ::: BaseUrl] =>
  Org.Organization ->
  WithBecknCallbackMig api callback_success m
withCallback' doWithCallback transporter action api context cbUrl f = do
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
  bppUri <- makeBppUrl (transporter.id)
  let context' =
        context
          & #bpp_uri ?~ bppUri
          & #bpp_id ?~ transporter.shortId.getShortId
  withBecknCallbackMig doWithCallback (Just authKey) action api context' cbUrl f

callOnSelect ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Org.Organization ->
  DSR.SearchRequest ->
  OnSelect.OnSelectMessage ->
  m ()
callOnSelect transporter searchRequest content = do
  let bapId = searchRequest.bapId
      bapUri = searchRequest.bapUri
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
  bppUri <- makeBppUrl (transporter.id)
  let msgId = searchRequest.messageId
  context <- buildTaxiContext Context.ON_SELECT msgId Nothing bapId bapUri (Just transporter.shortId.getShortId) (Just bppUri)
  logDebug $ "on_select request bpp: " <> show content
  void . Beckn.callBecknAPI (Just authKey) Nothing (show Context.ON_SELECT) API.onSelectAPI bapUri . BecknCallbackReq context $ Right content

callOnUpdate ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Org.Organization ->
  DRB.Booking ->
  OnUpdate.OnUpdateMessage ->
  m ()
callOnUpdate transporter booking content = do
  let bapId = booking.bapId
      bapUri = booking.bapUri
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
  bppUri <- makeBppUrl (transporter.id)
  msgId <- generateGUID
  context <- buildTaxiContext Context.ON_UPDATE msgId Nothing bapId bapUri (Just transporter.shortId.getShortId) (Just bppUri)
  void . Beckn.callBecknAPI (Just authKey) Nothing (show Context.ON_UPDATE) API.onUpdateAPI bapUri . BecknCallbackReq context $ Right content

callOnConfirm ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Org.Organization ->
  Context.Context ->
  OnConfirm.OnConfirmMessage ->
  m ()
callOnConfirm transporter contextFromConfirm content = do
  let bapUri = contextFromConfirm.bap_uri
      bapId = contextFromConfirm.bap_id
      msgId = contextFromConfirm.message_id
      bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
  bppUri <- makeBppUrl transporter.id
  context_ <- buildTaxiContext Context.ON_CONFIRM msgId Nothing bapId bapUri (Just transporter.shortId.getShortId) (Just bppUri)
  void $ Beckn.callBecknAPI (Just authKey) Nothing (show Context.ON_CONFIRM) API.onConfirmAPI bapUri . BecknCallbackReq context_ $ Right content

makeBppUrl ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Id Org.Organization ->
  m BaseUrl
makeBppUrl (Id transporterId) =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack transporterId)
