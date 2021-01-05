{-# LANGUAGE OverloadedLabels #-}

module FmdWrapper.Server where

import Beckn.Types.Core.Ack
import Beckn.Types.FMD.API.Search (OnSearchEndReq, OnSearchEndRes, OnSearchReq)
import qualified Beckn.Utils.SignatureAuth as HttpSig
import Control.Concurrent.Async
import EulerHS.Prelude
import FmdWrapper.Common
import Runner
import Servant

data CallbackData = CallbackData
  { onSearchTVar :: TVar [CallbackResult OnSearchReq],
    onSearchEndMVar :: MVar (CallbackResult OnSearchEndReq)
  }

withCallbackApp :: (CallbackData -> IO ()) -> IO ()
withCallbackApp action = do
  callbackData <- mkCallbackData
  withApp fmdTestAppPort (pure (callbackApp callbackData)) (action callbackData)

type CallbackAPI =
  "v1"
    :> ( OnSearchAPI
           :<|> OnSearchEndAPI
       )

type OnSearchAPI =
  "on_search"
    :> Header "Authorization" HttpSig.SignaturePayload
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] AckResponse

type OnSearchEndAPI =
  "on_search"
    :> "end"
    :> Header "Authorization" HttpSig.SignaturePayload
    :> ReqBody '[JSON] OnSearchEndReq
    :> Post '[JSON] OnSearchEndRes

callbackApp :: CallbackData -> Application
callbackApp callbackData = serve (Proxy :: Proxy CallbackAPI) $ callbackServer callbackData

callbackServer :: CallbackData -> Server CallbackAPI
callbackServer callbackData =
  onSearch callbackData
    :<|> onSearchEnd callbackData

onSearch :: CallbackData -> Maybe HttpSig.SignaturePayload -> OnSearchReq -> Handler AckResponse
onSearch callbackData sPayload req = do
  atomically $ modifyTVar (onSearchTVar callbackData) (CallbackResult (sPayload <&> (^. #params . #keyId . #subscriberId)) req :)
  pure $ AckResponse (req ^. #context) (ack "ACK") Nothing

onSearchEnd :: CallbackData -> Maybe HttpSig.SignaturePayload -> OnSearchEndReq -> Handler AckResponse
onSearchEnd callbackData sPayload req = do
  putMVar (onSearchEndMVar callbackData) $ CallbackResult (sPayload <&> (^. #params . #keyId . #subscriberId)) req
  pure $ AckResponse (req ^. #context) (ack "ACK") Nothing

mkCallbackData :: IO CallbackData
mkCallbackData = do
  onSearchTVar <- newTVarIO []
  onSearchEndMVar <- newEmptyMVar
  pure CallbackData {onSearchTVar = onSearchTVar, onSearchEndMVar = onSearchEndMVar}

waitForCallback :: MVar a -> IO (Maybe a)
waitForCallback mVar =
  rightToMaybe <$> race (threadDelay 5e6) (readMVar mVar)
