{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Engineering.Helpers.API where

import Prelude
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, makeRequest, Request(..), Header(..), Headers(..), ErrorResponse)
import Presto.Core.Types.Language.Flow (callAPI, APIResult(..), Flow)
import Common.Types.App (FlowBT)
import Engineering.Error.Utils
import Foreign.Generic (class Decode)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Debug (spy)
import Foreign.Object (empty)
import Control.Transformers.Back.Trans as App

callApi :: forall a b st.
  StandardEncode a =>
  Decode b =>
  RestEndpoint a b =>
  a ->
  Array Header ->
  Flow st (APIResult b)
callApi payload headers' = do
  logRequest
  result <- callAPI (Headers headers') payload
  case result of
    Right resp -> pure $ Right $ spy "Response :: " resp
    Left err -> do
      pure $ Left err
  where
    logRequest = do
      let (Request req) = makeRequest payload (Headers headers')
      void $ pure $ spy "Request :: " req

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

callGzipApi :: forall a b st.
  StandardEncode a =>
  Decode b =>
  RestEndpoint a b =>
  a ->
  Array Header ->
  Flow st (APIResult b)
callGzipApi payload headers = do
  let gzipHeader = [Header "Accept-Encoding" "gzip"]
  callApi payload (headers <> gzipHeader)

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
