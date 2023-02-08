module AWS.S3.Init where

import AWS.S3.Flow
import AWS.S3.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App (MonadFlow)

buildS3Env :: (MonadFlow m, CoreMetrics m) => S3Config -> S3Env m
buildS3Env (S3MockConf m) = do
  let baseDir = m.baseLocalDirectory
      bucketName = m.bucketName
  S3Env
    { pathPrefix = m.pathPrefix,
      getH = mockGet baseDir bucketName,
      putH = mockPut baseDir bucketName
    }
buildS3Env (S3AwsConf a) = do
  S3Env
    { pathPrefix = a.pathPrefix,
      getH = get'' a.bucketName,
      putH = put'' a.bucketName
    }
