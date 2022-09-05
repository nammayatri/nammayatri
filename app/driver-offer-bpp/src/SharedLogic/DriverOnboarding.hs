module SharedLogic.DriverOnboarding where

import AWS.S3 as S3
import Beckn.Prelude
import Beckn.Utils.Common
import Data.Text as T
import Data.Time.Format.ISO8601

createPath ::
  (HasFlowEnv m r '["s3Config" ::: S3.S3Config]) =>
  Text ->
  Text ->
  String ->
  m Text
createPath driverId orgId docType = do
  S3Config {..} <- asks (.s3Config)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return $
    T.pack
      ( "/" <> T.unpack pathPrefix <> "/driver-onboarding/" <> "org-" <> T.unpack orgId <> "/"
          <> T.unpack driverId
          <> "/"
          <> docType
          <> "/"
          <> T.unpack fileName
          <> ".png"
      )
