{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Types
  ( -- Storage configuration types
    StorageConfig (..),
    StorageProvider (..),
    StorageMode (..),
    -- Re-exported from AWS.S3.Types
    FileType (..),
    ObjectStatus (..),
    EntityTag (..),
    eTagToHash,
    -- Re-exported config types
    S3Config,
    GCSConfig,
  )
where

import AWS.S3.Types (EntityTag (..), FileType (..), ObjectStatus (..), S3Config, eTagToHash)
import GCP.GCS.Types (GCSConfig)
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)

-- | Multi-cloud storage configuration
-- Supports primary and optional secondary storage providers
data StorageConfig = StorageConfig
  { primaryStorage :: StorageProvider,
    secondaryStorage :: Maybe StorageProvider,
    enableMultiCloudWrite :: Bool -- If True, write to both primary and secondary
  }
  deriving (Generic, FromDhall)

-- | Storage provider type (S3 or GCS)
data StorageProvider = StorageS3 S3Config | StorageGCS GCSConfig
  deriving (Generic, FromDhall)

-- | Storage operation mode
data StorageMode = PrimaryOnly | SecondaryOnly | BothClouds
  deriving (Eq, Show)
