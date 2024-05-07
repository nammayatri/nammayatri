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
import Foreign.Generic (class Decode, decode)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Debug (spy)
import Foreign.Object (empty)
import Control.Transformers.Back.Trans as App
import Data.Function.Uncurried (Fn3, runFn3, Fn4, runFn4, Fn6, runFn6, runFn2)
import Engineering.Helpers.SQLiteUtils
import Data.Maybe
import Engineering.Helpers.Commons (liftFlow)
import JBridge
import Control.Monad.Except (runExcept)

callApi :: forall a b st.
  StandardEncode a =>
  Decode b =>
  RestEndpoint a b =>
  a ->
  Array Header ->
  Flow st (Either ErrorResponse b)
callApi payload headers' = do
  logRequest
  result <- callAPI (Headers headers') payload
  case result of
    Right resp -> pure $ Right $ spy "Response :: " resp.response
    Left err -> do
      pure $ Left err
  where
    logRequest = do
      let (Request req) = makeRequest payload (Headers headers')
      void $ pure $ spy "Request :: " req


-- callApiWithFallback :: forall a b c st.
--   StandardEncode a =>
--   -- Decode b =>
--   RestEndpoint a b =>
--   TableTransformer c b =>
--   a ->
--   Array Header ->
--   String ->
--   String ->
--   String ->
--   SqlSchema ->
--   Flow st (APIResult b)
-- callApiWithFallback payload headers' dbName tableName query tableSchema = do
--   logRequest
--   result <- callAPI (Headers headers') payload
--   case result of
--     Right resp -> do
--       void $ pure $ runFn2 deleteTable dbName tableName
--       void $ pure $ runFn3 createTable dbName tableName tableSchema
--       let (apiresp :: b)= resp.response
--       void $ pure $ addMultipleRowsToSQlite dbName tableName $ toTable apiresp
--       pure $ Right $ spy "Response :: " resp
--     Left err -> do
--       if (err.code == -1) then do 
--         liftFlow $ showInAppNotification inAppNotificationPayload{title = "No internet connection", message = "Please try again", channelId = "internetAction", showLoader = true, durationInMilliSeconds = 500000}
--       else pure unit
--       let cachedResp = runFn2 executeQuery dbName query 
--       case runExcept (decode cachedResp) of
--         Right ress -> do
--           void $ pure $ spy "sql Cached Response :: " ress
--           pure $ Right $ spy "sql Transformed Cached Response ::" $ fromTable $ ress
--         Left _ -> pure $ Left err
--   where
--     logRequest = do
--       let (Request req) = makeRequest payload (Headers headers')
--       void $ pure $ spy "Request :: " req

callAPIWithFallback payload headers' dbName transformToTable transformFromTable tableName query tableSchema deleteQuery = do
  logRequest
  result <- callAPI (Headers headers') payload
  case result of
    Right resp -> do
      let _ = runFn4 executeQuery dbName deleteQuery Just Nothing
      void $ pure $ addMultipleRowsToSQlite dbName tableName $ transformToTable resp.response
      pure $ Right $ spy "Response :: " resp.response
    Left err -> do
      _ <- pure $ spy "Error zxc :: " err
      if (err.code == -1) then do 
        liftFlow $ showInAppNotification inAppNotificationPayload{title = "No internet connection", message = "Please try again", channelId = "internetAction", showLoader = true, durationInMilliSeconds = 500000}
      else pure unit
      _ <- pure $ spy "before zxc :: cachedResp " err
      let cachedResp = runFn4 executeQuery dbName query Just Nothing
      _ <- pure $ spy "response zxc cr" cachedResp
      case (spy "case cachedResp " cachedResp) of
        Nothing -> pure $ Left err
        Just cachedRes -> do
          void $ pure $ spy "sql Cached Response :: " cachedRes
          pure $ Right $ spy "sql Transformed Cached Response ::" $ transformFromTable cachedRes
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
  Flow st (Either ErrorResponse b)
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
