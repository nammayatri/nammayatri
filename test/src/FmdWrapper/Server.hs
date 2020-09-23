{-# LANGUAGE OverloadedLabels #-}

module FmdWrapper.Server where

import Beckn.Types.Common
import Beckn.Types.FMD.API.Search (OnSearchEndReq, OnSearchEndRes, OnSearchReq)
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
    :> Header "X-API-Key" Text
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] AckResponse

type OnSearchEndAPI =
  "on_search"
    :> "end"
    :> Header "X-API-Key" Text
    :> ReqBody '[JSON] OnSearchEndReq
    :> Post '[JSON] OnSearchEndRes

callbackApp :: CallbackData -> Application
callbackApp callbackData = serve (Proxy :: Proxy CallbackAPI) $ callbackServer callbackData

callbackServer :: CallbackData -> Server CallbackAPI
callbackServer callbackData =
  onSearch callbackData
    :<|> onSearchEnd callbackData

onSearch :: CallbackData -> Maybe Text -> OnSearchReq -> Handler AckResponse
onSearch callbackData token req = do
  atomically $ modifyTVar (onSearchTVar callbackData) (CallbackResult token req :)
  pure $ AckResponse (req ^. #context) (ack "ACK") Nothing

onSearchEnd :: CallbackData -> Maybe Text -> OnSearchEndReq -> Handler AckResponse
onSearchEnd callbackData token req = do
  putMVar (onSearchEndMVar callbackData) $ CallbackResult token req
  pure $ AckResponse (req ^. #context) (ack "ACK") Nothing

mkCallbackData :: IO CallbackData
mkCallbackData = do
  onSearchTVar <- newTVarIO []
  onSearchEndMVar <- newEmptyMVar
  pure CallbackData {onSearchTVar = onSearchTVar, onSearchEndMVar = onSearchEndMVar}

waitForCallback :: MVar a -> IO (Maybe a)
waitForCallback mVar =
  rightToMaybe <$> race (threadDelay 5e6) (readMVar mVar)
