module Engineering.Helpers.API where

import Prelude
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, makeRequest, Request(..), Header(..), Headers(..))
import Presto.Core.Types.Language.Flow (callAPI, APIResult(..))
import Common.Types.App (FlowBT)
import Engineering.Error.Utils
import Foreign.Generic (class Decode)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Debug (spy)
import Control.Transformers.Back.Trans as App

callApiBT :: forall a b e st.
  ApiErrorHandler e st =>
  StandardEncode a =>
  Decode b =>
  RestEndpoint a b =>
  a ->
  Array Header ->
  e ->
  FlowBT String st b
callApiBT payload headers' errorHandler = do
  logRequest
  result <- lift $ lift $ callAPI (Headers headers') payload
  case result of
    Right resp -> pure $ spy "Response :: " resp.response
    Left err -> do
      handleApiError errorHandler $ spy "Error :: " err
      App.BackT $ pure App.GoBack
  where
    logRequest = do
      let (Request req) = makeRequest payload (Headers headers')
      void $ pure $ spy "Request :: " req

callGzipApiBT :: forall a b e st.
  ApiErrorHandler e st =>
  StandardEncode a =>
  Decode b =>
  RestEndpoint a b =>
  a ->
  Array Header ->
  e ->
  FlowBT String st b
callGzipApiBT payload headers errorHandler = do
  let gzipHeader = [Header "Accept-Encoding" "gzip"]
  callApiBT payload (headers <> gzipHeader) errorHandler
