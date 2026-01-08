{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module GCP.GCS.Types where

import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)

data GCSConfig = GCSConf GCSConfigDetails | GCSMockConf GCSMockConfig
  deriving (Generic, FromDhall)

data GCSConfigDetails = GCSConfigDetails
  { serviceAccountKey :: Text, -- JSON key as Text
    bucketName :: Text,
    projectId :: Text,
    pathPrefix :: Text
  }
  deriving (Generic, FromDhall)

data GCSMockConfig = GCSMockConfig
  { baseLocalDirectory :: String,
    pathPrefix :: Text,
    bucketName :: Text
  }
  deriving (Generic, FromDhall)
