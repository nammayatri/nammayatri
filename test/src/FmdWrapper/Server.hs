module FmdWrapper.Server where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Order
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Utils.SignatureAuth as HttpSig
import Control.Concurrent.MVar (isEmptyMVar, modifyMVar_)
import EulerHS.Prelude
import FmdWrapper.Common
import Runner
import Servant
import qualified "fmd-wrapper" Types.Beckn.API.Cancel as CancelAPI
import qualified "fmd-wrapper" Types.Beckn.API.Confirm as ConfirmAPI
import qualified "fmd-wrapper" Types.Beckn.API.Search as SearchAPI
import qualified "fmd-wrapper" Types.Beckn.API.Status as StatusAPI
import qualified "fmd-wrapper" Types.Beckn.API.Track as TrackAPI

data CallbackData = CallbackData
  { onSearchCb :: MVar [CallbackResult (BecknCallbackReq SearchAPI.OnSearchCatalog)],
    onConfirmCb :: MVar (CallbackResult (BecknCallbackReq OrderObject)),
    onStatusCb :: MVar (CallbackResult (BecknCallbackReq OrderObject)),
    onTrackCb :: MVar (CallbackResult (BecknCallbackReq TrackAPI.OnTrackInfo)),
    onCancelCb :: MVar (CallbackResult (BecknCallbackReq OrderObject))
  }

withCallbackApp :: (CallbackData -> IO ()) -> IO ()
withCallbackApp action = do
  callbackData <- mkCallbackData
  withApp fmdTestAppPort (pure (callbackApp callbackData)) (action callbackData)

callbackApp :: CallbackData -> Application
callbackApp callbackData = serve (Proxy :: Proxy CallbackAPI) $ callbackServer callbackData

mkCallbackData :: IO CallbackData
mkCallbackData =
  CallbackData
    <$> newMVar []
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar

waitForCallback :: IO ()
waitForCallback = do
  threadDelay 5e6

type CallbackAPI =
  "v1"
    :> ( OnSearchAPI
           :<|> OnConfirmAPI
           :<|> OnTrackAPI
           :<|> OnStatusAPI
           :<|> OnCancelAPI
       )

callbackServer :: CallbackData -> Server CallbackAPI
callbackServer cbData =
  onSearch cbData
    :<|> onConfirm cbData
    :<|> onTrack cbData
    :<|> onStatus cbData
    :<|> onCancel cbData

type OnSearchAPI =
  Header "Authorization" HttpSig.SignaturePayload
    :> SearchAPI.OnSearchAPI

onSearch :: CallbackData -> Maybe HttpSig.SignaturePayload -> BecknCallbackReq SearchAPI.OnSearchCatalog -> Handler AckResponse
onSearch callbackData sPayload req = do
  liftIO $ modifyMVar_ (onSearchCb callbackData) (pure . (CallbackResult (sPayload <&> (.params.keyId.subscriberId)) req :))
  pure Ack

type OnConfirmAPI =
  Header "Authorization" HttpSig.SignaturePayload
    :> ConfirmAPI.OnConfirmAPI

onConfirm :: CallbackData -> Maybe HttpSig.SignaturePayload -> BecknCallbackReq OrderObject -> Handler AckResponse
onConfirm callbackData sPayload req = do
  putOrSwapMVar (callbackData.onConfirmCb) (CallbackResult (sPayload <&> (.params.keyId.subscriberId)) req)
  pure Ack

type OnTrackAPI =
  Header "Authorization" HttpSig.SignaturePayload
    :> TrackAPI.OnTrackAPI

onTrack :: CallbackData -> Maybe HttpSig.SignaturePayload -> BecknCallbackReq TrackAPI.OnTrackInfo -> Handler AckResponse
onTrack callbackData sPayload req = do
  putOrSwapMVar (callbackData.onTrackCb) (CallbackResult (sPayload <&> (.params.keyId.subscriberId)) req)
  pure Ack

type OnStatusAPI =
  Header "Authorization" HttpSig.SignaturePayload
    :> StatusAPI.OnStatusAPI

onStatus :: CallbackData -> Maybe HttpSig.SignaturePayload -> BecknCallbackReq OrderObject -> Handler AckResponse
onStatus callbackData sPayload req = do
  putOrSwapMVar (callbackData.onStatusCb) (CallbackResult (sPayload <&> (.params.keyId.subscriberId)) req)
  pure Ack

type OnCancelAPI =
  Header "Authorization" HttpSig.SignaturePayload
    :> CancelAPI.OnCancelAPI

onCancel :: CallbackData -> Maybe HttpSig.SignaturePayload -> BecknCallbackReq OrderObject -> Handler AckResponse
onCancel callbackData sPayload req = do
  putOrSwapMVar (callbackData.onCancelCb) (CallbackResult (sPayload <&> (.params.keyId.subscriberId)) req)
  pure Ack

putOrSwapMVar :: MVar a -> a -> Handler ()
putOrSwapMVar mVar f = do
  isEmpty <- liftIO $ isEmptyMVar mVar
  if isEmpty
    then putMVar mVar f
    else void $ swapMVar mVar f
