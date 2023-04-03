{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AWS.S3.SignatureAuth
  ( prepareS3AuthManager,
    modFlowRtWithS3AuthManagers,
    mkS3MbManager,
  )
where

import AWS.S3.Types
import AWS.S3.Utils
import Data.ByteString as DB
import Data.ByteString.Char8 as DBC
import Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI (foldedCase)
import qualified Data.HashMap.Internal as HMap
import qualified Data.List as DL
import Data.Map
import qualified Data.HashMap.Strict as HMS
import Data.Text.Encoding as DTE
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Kernel.Types.App
import Kernel.Types.Flow
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (HasLog)
import Network.HTTP.Client as Http
import Network.HTTP.Client.TLS as Http
import Network.HTTP.Types as HttpTypes
import qualified Network.URI.Encode as URIEncode

fetchReqData :: Http.Request -> DB.ByteString -> S3AuthParams
fetchReqData req reqBody =
  S3AuthParams
    { headers,
      queryString = Http.queryString req,
      body = reqBody,
      path = Http.path req,
      method = Http.method req,
      date
    }
  where
    headers = Http.requestHeaders req
    dateHeaderConst = DBC.pack "x-amz-date"
    date = maybe DBC.empty snd $ DL.find ((==) dateHeaderConst . CI.foldedCase . fst) headers

encodeHeaders :: HttpTypes.RequestHeaders -> (DB.ByteString, DB.ByteString)
encodeHeaders reqHeaders = do
  let headers = sortAndGroup (foldHeader <$> reqHeaders)
      canonicalHeaders = DBC.intercalate (DBC.singleton '\n') (canonicalFormat <$> headers)
      signedHeaders = DBC.intercalate (DBC.singleton ';') (signedFormat <$> headers)
  (canonicalHeaders, signedHeaders)
  where
    canonicalFormat header = fst header <> DBC.pack ":" <> snd header
    signedFormat = fst
    foldHeader (name, value) = (CI.foldedCase name, value)
    combineHeader a b = a <> "," <> b
    sortAndGroup = assocs . fromListWith combineHeader

encodeQueryString :: DB.ByteString -> DB.ByteString
encodeQueryString (DBC.uncons -> Nothing) = DBC.empty
encodeQueryString qs =
  ( DBC.intercalate (DBC.singleton '&')
      . DL.map (merge . DBC.split '=')
      . DBC.split '&'
      . maybe DBC.empty snd
      . DBC.uncons
  )
    qs
  where
    merge [x] = uriEnc x <> DBC.singleton '=' <> DBC.empty
    merge [x, y] = uriEnc x <> DBC.singleton '=' <> uriEnc y
    merge _ = DBC.empty
    uriEnc = URIEncode.encodeByteString

mkCanonicalReq :: S3AuthParams -> DB.ByteString
mkCanonicalReq params =
  hexSHA256 $
    DBC.concat
      [ params.method,
        nc,
        params.path,
        nc,
        encodeQueryString params.queryString,
        nc,
        headers,
        nc,
        nc,
        signedHeaders,
        nc,
        hexSHA256 params.body
      ]
  where
    (headers, signedHeaders) = encodeHeaders params.headers

mkScopeStr :: DB.ByteString -> DB.ByteString -> DB.ByteString
mkScopeStr date region = do
  let scopeArray = [DBC.take 8 date, region, DBC.pack "s3/aws4_request"]
  DBC.intercalate (DBC.singleton '/') scopeArray

mkStringToSign :: S3AuthParams -> DB.ByteString -> DB.ByteString
mkStringToSign params region =
  DBC.concat [algorithm, nc, params.date, nc, scope, nc, mkCanonicalReq params]
  where
    scope = mkScopeStr params.date region

algorithm :: DB.ByteString
algorithm = DBC.pack "AWS4-HMAC-SHA256"

nc :: DB.ByteString
nc = DBC.singleton '\n'

mkSigningKey :: DB.ByteString -> DB.ByteString -> DB.ByteString -> DB.ByteString
mkSigningKey secretKey date region =
  hmacSHA256_64 (hmacSHA256_64 (hmacSHA256_64 (hmacSHA256_64 aws4SKey fDate) region) service) (DBC.pack "aws4_request")
  where
    aws4SKey = DBC.pack "AWS4" <> secretKey
    service = DBC.pack "s3"
    fDate = DBC.take 8 date

mkAuthorizationString :: S3AuthParams -> DBC.ByteString -> DB.ByteString -> DB.ByteString -> DB.ByteString
mkAuthorizationString params accessKeyId signature region =
  DBC.concat
    [ algorithm,
      DBC.pack " Credential=",
      accessKeyId,
      DBC.singleton '/',
      mkScopeStr params.date region,
      DBC.pack ", SignedHeaders=",
      snd $ encodeHeaders params.headers,
      DBC.pack ", Signature=",
      signature
    ]

prepareS3AuthManager ::
  HasLog r =>
  R.FlowRuntime ->
  r ->
  S3AwsConfig ->
  HMS.HashMap Text Http.ManagerSettings
prepareS3AuthManager flowRt appEnv s3AwsConfig =
  HMS.singleton s3AuthManagerKey $
    Http.tlsManagerSettings {Http.managerModifyRequest = runFlowR flowRt appEnv . doSignature}
  where
    doSignature req_ =
      withLogTag
        "prepareS3AuthManager"
        if isJust $ DL.lookup authHeaderName (Http.requestHeaders req_)
          then pure req_
          else do
            time <- getCurrentTime
            let reqBody = getBody $ Http.requestBody req_
            let amzHeaders =
                  [ ("X-Amz-Content-Sha256", hexSHA256 reqBody),
                    ("X-Amz-Date", DBC.pack $ formatISO8601 time),
                    ("Host", Http.host req_)
                  ]
            let req = DL.foldl addHeader req_ amzHeaders
            let params = fetchReqData req reqBody
            let stringToSign = mkStringToSign params region
            let signingKey = mkSigningKey secretAccessKey params.date region
            let signature = hex $ hmacSHA256_64 signingKey stringToSign
            let authString = mkAuthorizationString params accessKeyId signature region
            logDebug $ "Request headers: " +|| params.headers ||+ ""
            pure $ addHeader req (authHeaderName, authString)

    accessKeyId = DTE.encodeUtf8 s3AwsConfig.accessKeyId
    secretAccessKey = DTE.encodeUtf8 s3AwsConfig.secretAccessKey
    region = DTE.encodeUtf8 $ s3AwsConfig.region

    getBody (Http.RequestBodyLBS body) = BSL.toStrict body
    getBody (Http.RequestBodyBS body) = body
    getBody _ = ""

    authHeaderName = "Authorization"
    addHeader req header = do
      let headers = Http.requestHeaders req
      req {Http.requestHeaders = header : headers}

modFlowRtWithS3AuthManagers ::
  ( HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    MonadReader r m,
    HasLog r,
    MonadFlow m
  ) =>
  R.FlowRuntime ->
  r ->
  S3AwsConfig ->
  m R.FlowRuntime
modFlowRtWithS3AuthManagers flowRt appEnv s3AwsConfig = do
  let managersSetting = prepareS3AuthManager flowRt appEnv s3AwsConfig
  managers <- createManagers managersSetting
  logInfo "Loaded S3 http manager - "
  pure $ flowRt {R._httpClientManagers = managers}

mkS3MbManager :: (HasLog r) => R.FlowRuntime -> r -> S3Config -> Maybe (HMS.HashMap Text ManagerSettings)
mkS3MbManager _flowRt _appEnv S3MockConf {} = Nothing
mkS3MbManager flowRt appEnv (S3AwsConf s3AwsConfig) = Just $ prepareS3AuthManager flowRt appEnv s3AwsConfig
