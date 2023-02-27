{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Idfy.WebhookHandler where

import Data.Aeson.Types as DAT
import qualified Data.Text as T
import Data.Time.Format
import EulerHS.Prelude
import Idfy.Auth
import qualified Idfy.External.Flow as EF
import Idfy.Types.IdfyConfig
import Idfy.Types.Request
import Idfy.Types.Response
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Flow
import Kernel.Utils.Common
import Kernel.Utils.IOLogging

webhookHandler ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    HasField "idfyCfg" a IdfyConfig
  ) =>
  (VerificationResponse -> Text -> FlowR a AckResponse) ->
  Maybe Text ->
  Value ->
  FlowHandlerR a AckResponse
webhookHandler verifyHandler secret val = withFlowHandlerAPI $ do
  withLogTag "webhookIdfy" $ do
    let respDump = encodeToText val
    let mResp = fromJSON val
    case mResp of
      DAT.Success (resp :: VerificationResponse) -> do
        void $ verifyAuth secret
        void $ verifyHandler resp respDump
        pure Ack
      DAT.Error err1 -> do
        logInfo $ "Error 1: " <> show err1
        let mRespList = fromJSON val
        case mRespList of
          DAT.Success (respList :: VerificationResponseList) -> do
            let mResp_ = listToMaybe respList
            case mResp_ of
              Just resp_ -> do
                void $ verifyAuth secret
                void $ verifyHandler resp_ respDump
                pure Ack
              Nothing -> pure Ack
          DAT.Error err -> do
            logInfo $ "Error 2: " <> show err
            pure Ack

verifyDL ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    CoreMetrics (FlowR a),
    HasField "idfyCfg" a IdfyConfig
  ) =>
  Text ->
  UTCTime ->
  FlowR a IdfySuccess
verifyDL dlId dob = do
  idfyConfig <- asks (.idfyCfg)
  let dobDay = T.pack $ formatTime defaultTimeLocale "%F" dob
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        IdfyRequest
          { _data =
              DLVerificationData
                { id_number = dlId,
                  date_of_birth = dobDay
                },
            ..
          }
  EF.verifyDLAsync idfyConfig.api_key idfyConfig.account_id idfyConfig.url req

verifyRC ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    CoreMetrics (FlowR a),
    HasField "idfyCfg" a IdfyConfig
  ) =>
  Text ->
  FlowR a IdfySuccess
verifyRC rcId = do
  idfyConfig <- asks (.idfyCfg)
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        IdfyRequest
          { _data = RCVerificationData rcId Nothing,
            ..
          }
  EF.verifyRCAsync idfyConfig.api_key idfyConfig.account_id idfyConfig.url req

validateImage ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    CoreMetrics (FlowR a),
    HasField "idfyCfg" a IdfyConfig
  ) =>
  Text ->
  Text ->
  FlowR a ImageValidateResponse
validateImage doc1 doctype = do
  idfyConfig <- asks (.idfyCfg)
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        IdfyRequest
          { _data =
              ValidateRequest
                { document1 = doc1,
                  doc_type = doctype
                },
            ..
          }
  EF.validateImage idfyConfig.api_key idfyConfig.account_id idfyConfig.url req

extractRCImage ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    CoreMetrics (FlowR a),
    HasField "idfyCfg" a IdfyConfig
  ) =>
  Text ->
  Maybe Text ->
  FlowR a RCExtractResponse
extractRCImage doc1 mbdoc2 = do
  idfyConfig <- asks (.idfyCfg)
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        IdfyRequest
          { _data =
              ExtractRequest
                { document1 = doc1,
                  document2 = mbdoc2
                },
            ..
          }
  EF.extractRCImage idfyConfig.api_key idfyConfig.account_id idfyConfig.url req

extractDLImage ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    CoreMetrics (FlowR a),
    HasField "idfyCfg" a IdfyConfig
  ) =>
  Text ->
  Maybe Text ->
  FlowR a DLExtractResponse
extractDLImage doc1 mbdoc2 = do
  idfyConfig <- asks (.idfyCfg)
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        IdfyRequest
          { _data =
              ExtractRequest
                { document1 = doc1,
                  document2 = mbdoc2
                },
            ..
          }
  EF.extractDLImage idfyConfig.api_key idfyConfig.account_id idfyConfig.url req

getTask ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    CoreMetrics (FlowR a),
    HasField "idfyCfg" a IdfyConfig
  ) =>
  Text ->
  FlowR a VerificationResponse
getTask request_id = do
  idfyConfig <- asks (.idfyCfg)
  EF.getTask idfyConfig.api_key idfyConfig.account_id idfyConfig.url request_id
