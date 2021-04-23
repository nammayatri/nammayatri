{-# LANGUAGE OverloadedLabels #-}

module FmdWrapper.Server where

import Beckn.Types.Core.Ack
import Beckn.Types.FMD.API.Search (OnSearchReq)
import qualified Beckn.Utils.SignatureAuth as HttpSig
import EulerHS.Prelude
import FmdWrapper.Common
import Runner
import Servant

newtype CallbackData = CallbackData
  { onSearchTVar :: TVar [CallbackResult OnSearchReq]
  }

withCallbackApp :: (CallbackData -> IO ()) -> IO ()
withCallbackApp action = do
  callbackData <- mkCallbackData
  withApp fmdTestAppPort (pure (callbackApp callbackData)) (action callbackData)

type CallbackAPI =
  "v1"
    :> OnSearchAPI

type OnSearchAPI =
  "on_search"
    :> Header "Authorization" HttpSig.SignaturePayload
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] AckResponse

callbackApp :: CallbackData -> Application
callbackApp callbackData = serve (Proxy :: Proxy CallbackAPI) $ callbackServer callbackData

callbackServer :: CallbackData -> Server CallbackAPI
callbackServer = onSearch

onSearch :: CallbackData -> Maybe HttpSig.SignaturePayload -> OnSearchReq -> Handler AckResponse
onSearch callbackData sPayload req = do
  atomically $ modifyTVar (onSearchTVar callbackData) (CallbackResult (sPayload <&> (^. #params . #keyId . #subscriberId)) req :)
  pure $ AckResponse (req ^. #context) (ack ACK) Nothing

mkCallbackData :: IO CallbackData
mkCallbackData = do
  onSearchTVar <- newTVarIO []
  pure $ CallbackData onSearchTVar

waitForCallback :: IO ()
waitForCallback = do
  threadDelay 5e6
