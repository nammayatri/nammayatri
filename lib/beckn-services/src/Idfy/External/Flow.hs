module Idfy.External.Flow
  ( verifyDLAsync,
    verifyRCAsync,
    validateImage,
    extractRCImage,
    extractDLImage,
  )
where

import Beckn.Tools.Metrics.CoreMetrics.Types
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common hiding (Error)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Idfy.Types.Error
import Idfy.Types.ExtractImage
import Idfy.Types.IdfyConfig
import Idfy.Types.IdfyRes
import Idfy.Types.ValidateImage
import Idfy.Types.VerifyReq
import Servant (Header, JSON, Post, ReqBody, (:>))
import Servant.Client (ClientError)

type VerifyDLAPI =
  "v3" :> "tasks" :> "async" :> "verify_with_source" :> "ind_driving_license"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] (VerifyReq VerifyDLData)
    :> Post '[JSON] IdfyRes

verifyDLAPI :: Proxy VerifyDLAPI
verifyDLAPI = Proxy

verifyDLAsync ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  (VerifyReq VerifyDLData) ->
  m IdfyRes
verifyDLAsync apiKey accountId url req = callIdfyAPI url task "verifyDLAsync"
  where
    task =
      T.client
        verifyDLAPI
        (Just apiKey)
        (Just accountId)
        req

type VerifyRCAPI =
  "v3" :> "tasks" :> "async" :> "verify_with_source" :> "ind_rc_basic"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] (VerifyReq VerifyRCData)
    :> Post '[JSON] IdfyRes

verifyRCAPI :: Proxy VerifyRCAPI
verifyRCAPI = Proxy

verifyRCAsync ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  (VerifyReq VerifyRCData) ->
  m IdfyRes
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
    :> ReqBody '[JSON] ValidateImageReq
    :> Post '[JSON] ValidateImageRes

validateImageAPI :: Proxy ValidateImage
validateImageAPI = Proxy

validateImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  ValidateImageReq ->
  m StatusCheck
validateImage apiKey accountId url req = callAPI url task "validateImage" >>= checkIdfyError url
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
    :> ReqBody '[JSON] ExtractImageReq
    :> Post '[JSON] ExtractedRCDetails

extractRCAPI :: Proxy ExtractRCAPI
extractRCAPI = Proxy

extractRCImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  ExtractImageReq ->
  m StatusCheck
extractRCImage apiKey accountId url req = callAPI url task "extractRCImage" >>= checkIdfyError url
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
    :> ReqBody '[JSON] ExtractImageReq
    :> Post '[JSON] ExtractedDLDetails

extractDLAPI :: Proxy ExtractDLImage
extractDLAPI = Proxy

extractDLImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  ExtractImageReq ->
  m StatusCheck
extractDLImage apiKey accountId url req = callAPI url task "extractDLImage" >>= checkIdfyError url
  where
    task =
      T.client
        extractDLAPI
        (Just apiKey)
        (Just accountId)
        req

idfyError :: BaseUrl -> ClientError -> ExternalAPICallError
idfyError = ExternalAPICallError (Just "IDFY_API_ERROR")

checkIdfyError :: (MonadThrow m, Log m, HasField "status" a Text) => BaseUrl -> Either ClientError a -> m StatusCheck
checkIdfyError url res =
  fromEitherM (idfyError url) res >>= validateResponseStatus

validateResponseStatus :: (MonadThrow m, Log m, HasField "status" a Text) => a -> m StatusCheck
validateResponseStatus response =
  case response.status of
    "completed" -> return VALID
    "failed" -> return INVALID
    _ -> throwError IdfyServerError

callIdfyAPI :: CallAPI env res
callIdfyAPI = callApiUnwrappingApiError (identity @Error) Nothing Nothing