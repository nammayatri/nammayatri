{-# LANGUAGE OverloadedLabels #-}

module ExternalAPI.Flow where

import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes (BecknCallbackReq (BecknCallbackReq))
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as API
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import Beckn.Types.Id
import Beckn.Utils.Callback (WithBecknCallbackMig, withBecknCallbackMig)
import qualified Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Beckn.Utils.Servant.SignatureAuth
import Control.Lens.Operators ((?~))
import qualified Data.Text as T
import Domain.Types.Organization as Org
import qualified Domain.Types.RideBooking as DRB
import EulerHS.Prelude
import Tools.Metrics (CoreMetrics)
import Utils.Common

withCallback ::
  HasFlowEnv m r '["nwAddress" ::: BaseUrl] =>
  Org.Organization ->
  WithBecknCallbackMig api callback_success m
withCallback = withCallback' identity

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

callOnUpdate ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Org.Organization ->
  DRB.RideBooking ->
  OnUpdate.OnUpdateMessage ->
  m ()
callOnUpdate transporter rideBooking content = do
  let bapId = rideBooking.bapId
      bapUri = rideBooking.bapUri
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
  bppUri <- makeBppUrl (transporter.id)
  msgId <- generateGUID
  context <- buildTaxiContext Context.ON_UPDATE msgId Nothing bapId bapUri (Just transporter.shortId.getShortId) (Just bppUri)
  void . Beckn.callBecknAPI (Just authKey) Nothing (show Context.ON_UPDATE) API.onUpdateAPI bapUri . BecknCallbackReq context $ Right content

makeBppUrl ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Id Org.Organization ->
  m BaseUrl
makeBppUrl (Id transporterId) =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack transporterId)
