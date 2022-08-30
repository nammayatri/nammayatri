
module Idfy.Flow where

import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.Common
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Idfy.Types hiding (Error)
import Servant (Header, JSON, Post, ReqBody, (:>))
import Servant.Client (ClientError)
import Beckn.Types.Error
import Idfy.Types (Error)
import Data.OpenApi (ToSchema)
import Beckn.Utils.Common hiding (Error)

data StatusCheck = VALID | INVALID deriving (Generic, ToJSON, Show, FromJSON, ToSchema)

type ValidateRCImage = 
  "v3" :> "tasks" :> "sync" :> "validate" :> "document"
  :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] ValidateReq
    :> Post '[JSON] ValidateRes

validateRCAPI :: Proxy ValidateRCImage
validateRCAPI = Proxy

validateRCImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  ValidateReq ->
  m StatusCheck
validateRCImage apiKey accountId url req = callAPI url task "validateRCImage" >>= checkIdfyError url
  where
    task =
      T.client
        validateRCAPI
        (Just apiKey)
        (Just accountId)
        req

type ExtractRCAPI =
  "v3" :> "tasks" :> "sync" :> "extract" :> "ind_rc"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] VerifyRCReq
    :> Post '[JSON] VerifyRCSyncRes

extractRCAPI :: Proxy ExtractRCAPI
extractRCAPI = Proxy

extractRCImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  VerifyRCReq ->
  m StatusCheck
extractRCImage apiKey accountId url req = callAPI url task "extractRCImage" >>= checkIdfyError url
  where
    task =
      T.client
        extractRCAPI
        (Just apiKey)
        (Just accountId)
        req

type VerifyRCAPI =
  "v3" :> "tasks" :> "async" :> "verify_with_source" :> "ind_rc_basic"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] VerifyRCwithSourceReq
    :> Post '[JSON] VerifyIdfyRCAsyncRes

verifyRCAPI :: Proxy VerifyRCAPI
verifyRCAPI = Proxy

verifyRC ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  VerifyRCwithSourceReq ->
  m VerifyIdfyRCAsyncRes
verifyRC apiKey accountId url req = callIdfyAPI url task "verifyRC"
  where
    task =
      T.client
        verifyRCAPI
        (Just apiKey)
        (Just accountId)
        req


type ValidateDLImage = 
  "v3" :> "tasks" :> "sync" :> "validate" :> "document"
  :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] ValidateReq
    :> Post '[JSON] ValidateRes

validateDLAPI :: Proxy ValidateDLImage
validateDLAPI = Proxy

validateDLImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  ValidateReq ->
  m StatusCheck
validateDLImage apiKey accountId url req = callAPI url task "validateDLImage" >>= checkIdfyError url
  where
    task =
      T.client
        validateRCAPI
        (Just apiKey)
        (Just accountId)
        req

type ExtractDLImage =
  "v3" :> "tasks" :> "sync" :> "extract" :> "ind_driving_license"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] VerifyDLReq
    :> Post '[JSON] VerifyDLSyncRes

extractDLAPI :: Proxy ExtractDLImage
extractDLAPI = Proxy

extractDLImage ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  VerifyDLReq ->
  m StatusCheck
extractDLImage apiKey accountId url req = callAPI url task "extractDLImage" >>= checkIdfyError url
  where
    task =
      T.client
        extractDLAPI
        (Just apiKey)
        (Just accountId)
        req

type VerifyDLAPI =
  "v3" :> "tasks" :> "async" :> "verify_with_source" :> "ind_driving_license"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] VerifyDLwithSourceReq
    :> Post '[JSON] VerifyDLAsyncData

verifyDLwithSourceAPI :: Proxy VerifyDLAPI
verifyDLwithSourceAPI = Proxy

verifyDL ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  VerifyDLwithSourceReq ->
  m VerifyDLAsyncData
verifyDL  apiKey accountId url req = callIdfyAPI url task "verifyDL"
  where
    task =
      T.client
        verifyDLwithSourceAPI
        (Just apiKey)
        (Just accountId)
        req

checkIdfyError :: (MonadThrow m, Log m, HasField "status" a Text) => BaseUrl -> Either ClientError a -> m StatusCheck
checkIdfyError url res =
  fromEitherM (idfyError url) res >>= validateResponseStatus

idfyError :: BaseUrl -> ClientError -> ExternalAPICallError
idfyError = ExternalAPICallError (Just "IDFY_API_ERROR")

validateResponseStatus :: (MonadThrow m, Log m, HasField "status" a Text) => a -> m StatusCheck
validateResponseStatus response =
  case response.status of
    "completed" -> return VALID
    "failed" -> return INVALID
    _ -> throwError IdfyServerError

callIdfyAPI :: CallAPI env res
callIdfyAPI = callApiUnwrappingApiError (identity @Error) Nothing Nothing
