{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AWS.S3.Init where

import AWS.S3.Flow
import AWS.S3.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

buildS3Env :: (MonadFlow m, CoreMetrics m) => S3Config -> S3Env m
buildS3Env (S3MockConf m) = do
  let baseDir = m.baseLocalDirectory
      bucketName = m.bucketName
  S3Env
    { pathPrefix = m.pathPrefix,
      getH = mockGet baseDir bucketName,
      putH = mockPut baseDir bucketName,
      putRawH = mockPutRaw baseDir bucketName,
      deleteH = mockDelete baseDir bucketName,
      generateUploadUrlH = mockGenerateUploadUrl baseDir bucketName,
      generateDownloadUrlH = mockGenerateDownloadUrl baseDir bucketName,
      headRequestH = mockHeadRequest baseDir bucketName
    }
buildS3Env (S3AwsConf a) = do
  S3Env
    { pathPrefix = a.pathPrefix,
      getH = get'' a.bucketName,
      putH = put'' a.bucketName,
      putRawH = putRaw'' a.bucketName,
      deleteH = delete'' a.bucketName,
      generateUploadUrlH = generateUploadUrl' a.bucketName,
      generateDownloadUrlH = generateDownloadUrl' a.bucketName,
      headRequestH = headRequest' a.bucketName
    }
