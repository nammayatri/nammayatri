module Idfy.External.Flow
  ( verifyDLAsync,
    verifyRCAsync,
    validateImage,
    extractRCImage,
    extractDLImage,
    getTask,
    VerifyDLAPI,
    VerifyRCAPI,
    ValidateImage,
    ExtractDLImage,
    ExtractRCAPI,
  )
where

import EulerHS.Prelude
import qualified EulerHS.Types as T
import Idfy.Auth
import Idfy.Types.Error
import Idfy.Types.IdfyConfig
import Idfy.Types.Request
import Idfy.Types.Response
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import Kernel.Utils.Common hiding (Error)
import Servant (Get, Header, JSON, Post, ReqBody, (:>))

type VerifyDLAPI =
  "v3" :> "tasks" :> "async" :> "verify_with_source" :> "ind_driving_license"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] DLVerificationRequest
    :> Post '[JSON] IdfySuccess

verifyDLAPI :: Proxy VerifyDLAPI
verifyDLAPI = Proxy

verifyDLAsync ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  DLVerificationRequest ->
  m IdfySuccess
verifyDLAsync apiKey accountId url req = callIdfyAPI url task "verifyDLAsync"
  where
    task =
      T.client
        verifyDLAPI
        (Just apiKey)
        (Just accountId)
        req

type VerifyRCAPI =
  "v3" :> "tasks" :> "async" :> "verify_with_source" :> "ind_rc_plus"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] RCVerificationRequest
    :> Post '[JSON] IdfySuccess

verifyRCAPI :: Proxy VerifyRCAPI
verifyRCAPI = Proxy

verifyRCAsync ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  RCVerificationRequest ->
  m IdfySuccess
verifyRCAsync apiKey accountId url req = callIdfyAPI url task "verifyRCAsync"
  where
    task =
      T.client
        verifyRCAPI
        (Just apiKey)
        (Just accountId)
        req

type ValidateImage =
  "v3" :> "tasks" :> "sync" :> "validate" :> "document"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] ImageValidateRequest
    :> Post '[JSON] ImageValidateResponse

validateImageAPI :: Proxy ValidateImage
validateImageAPI = Proxy

validateImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  ImageValidateRequest ->
  m ImageValidateResponse
validateImage apiKey accountId url req = callIdfyAPI url task "validateImage"
  where
    task =
      T.client
        validateImageAPI
        (Just apiKey)
        (Just accountId)
        req

type ExtractRCAPI =
  "v3" :> "tasks" :> "sync" :> "extract" :> "ind_rc"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] ImageExtractRequest
    :> Post '[JSON] RCExtractResponse

extractRCAPI :: Proxy ExtractRCAPI
extractRCAPI = Proxy

extractRCImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  ImageExtractRequest ->
  m RCExtractResponse
extractRCImage apiKey accountId url req = callIdfyAPI url task "extractRCImage"
  where
    task =
      T.client
        extractRCAPI
        (Just apiKey)
        (Just accountId)
        req

type ExtractDLImage =
  "v3" :> "tasks" :> "sync" :> "extract" :> "ind_driving_license"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] ImageExtractRequest
    :> Post '[JSON] DLExtractResponse

extractDLAPI :: Proxy ExtractDLImage
extractDLAPI = Proxy

extractDLImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  ImageExtractRequest ->
  m DLExtractResponse
extractDLImage apiKey accountId url req = callIdfyAPI url task "extractDLImage"
  where
    task =
      T.client
        extractDLAPI
        (Just apiKey)
        (Just accountId)
        req

type GetTaskAPI =
  "v3" :> "tasks"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> MandatoryQueryParam "request_id" Text
    :> Get '[JSON] VerificationResponse

getTaskApi :: Proxy GetTaskAPI
getTaskApi = Proxy

getTask ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  Text ->
  m VerificationResponse
getTask apiKey accountId url request_id = callIdfyAPI url task "getTask"
  where
    task =
      T.client
        getTaskApi
        (Just apiKey)
        (Just accountId)
        request_id

callIdfyAPI :: CallAPI env res
callIdfyAPI = callApiUnwrappingApiError (identity @IdfyError) (Just idfyHttpManagerKey) (Just "IDFY_ERROR")
