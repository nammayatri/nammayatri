module Idfy.WebhookHandler where

import Beckn.Tools.Metrics.CoreMetrics.Types
import Beckn.Types.Flow
import Beckn.Utils.Common
import Beckn.Utils.IOLogging
import qualified Data.Text as T
import Data.Time.Format
import EulerHS.Prelude
import Idfy.Auth
import qualified Idfy.External.Flow as EF
import Idfy.Types.ExtractImage
import Idfy.Types.IdfyConfig
import Idfy.Types.IdfyRes
import Idfy.Types.ValidateImage
import Idfy.Types.VerificationResult
import Idfy.Types.VerifyReq

dlWebhookHandler ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    HasField "idfyCfg" a IdfyConfig
  ) =>
  (DLVerificationResponse -> FlowR a AckResponse) ->
  Maybe Text ->
  DLVerificationResponse ->
  FlowHandlerR a AckResponse
dlWebhookHandler handler secret result = withFlowHandlerAPI $ do
  void $ verifyAuth secret
  void $ handler result
  pure Ack

rcWebhookHandler ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    HasField "idfyCfg" a IdfyConfig
  ) =>
  (RCVerificationResponse -> FlowR a AckResponse) ->
  Maybe Text ->
  RCVerificationResponse ->
  FlowHandlerR a AckResponse
rcWebhookHandler handler secret result = withFlowHandlerAPI $ do
  void $ verifyAuth secret
  void $ handler result
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
  FlowR a IdfyRes
verifyDL dlId dob = do
  idfyConfig <- asks (.idfyCfg)
  let dobDay = T.pack $ formatTime defaultTimeLocale "%F" dob
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        VerifyReq
          { _data =
              VerifyDLData
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
  FlowR a IdfyRes
verifyRC rcId = do
  idfyConfig <- asks (.idfyCfg)
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        VerifyReq
          { _data = VerifyRCData rcId,
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
  FlowR a ValidateImageRes
validateImage doc1 doctype = do
  idfyConfig <- asks (.idfyCfg)
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        ValidateImageReq
          { _data =
              ValidateDoc
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
  FlowR a ExtractedRCDetails
extractRCImage doc1 mbdoc2 = do
  idfyConfig <- asks (.idfyCfg)
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        ExtractImageReq
          { _data =
              VerifyDocData
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
  FlowR a ExtractedDLDetails
extractDLImage doc1 mbdoc2 = do
  idfyConfig <- asks (.idfyCfg)
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        ExtractImageReq
          { _data =
              VerifyDocData
                { document1 = doc1,
                  document2 = mbdoc2
                },
            ..
          }
  EF.extractDLImage idfyConfig.api_key idfyConfig.account_id idfyConfig.url req
