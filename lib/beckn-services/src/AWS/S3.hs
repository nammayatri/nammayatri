module AWS.S3
  ( get,
    put,
    module S3Auth,
    module S3Types,
  )
where

import AWS.S3.Flow as S3Flow
import AWS.S3.SignatureAuth as S3Auth
import AWS.S3.Types as S3Types
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Utils.Common
import Data.Text as T
import EulerHS.Prelude hiding (decodeUtf8, get, put, show, traceShowId)

get ::
  ( CoreMetrics m,
    HasFlowEnv m r '["s3Config" ::: S3Config]
  ) =>
  String ->
  m Text
get path = do
  S3Config {..} <- asks (.s3Config)
  case accessKeyId of
    "mock-key" -> pure "Dummy Response"
    _ -> S3Flow.get' bucketName path

put ::
  ( CoreMetrics m,
    HasFlowEnv m r '["s3Config" ::: S3Config]
  ) =>
  String ->
  Text ->
  m Text
put path img = do
  withLogTag "S3" $ do
    S3Config {..} <- asks (.s3Config)
    case accessKeyId of
      "mock-key" -> pure "dummy-resp"
      _ -> S3Flow.put' bucketName path img
