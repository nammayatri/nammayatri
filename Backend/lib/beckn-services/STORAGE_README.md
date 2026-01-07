# Storage Multi-Cloud Module

Unified storage interface supporting AWS S3 and Google Cloud Storage (GCS) with multi-cloud write capabilities.

## Overview

The `Storage` module provides a unified API for file storage operations that works with both AWS S3 and GCP GCS. It supports:

- **Single cloud**: Use S3 or GCS as primary storage
- **Multi-cloud**: Write to both S3 and GCS simultaneously
- **Unified types**: All storage types exported from `Storage.Types`
- **Unified operations**: All operations through `Storage.Flow`

## Key Features

1. **Sequential Multi-Cloud Writes**: When multi-cloud is enabled, writes to primary first, then secondary
2. **Type Safety**: All types (`FileType`, `ObjectStatus`, `EntityTag`) come from `Storage.Types`
3. **Path Creation**: Unified `createFilePath` and `createFilePublicPath` functions
4. **Backward Compatible**: Legacy S3 code still works, but new code should use Storage

## Module Structure

```
Storage/
├── Types.hs      -- StorageConfig, StorageProvider, re-exported types
└── Flow.hs       -- Unified storage operations

GCP/
└── GCS/
    ├── Types.hs  -- GCS configuration types
    └── Flow.hs   -- GCS operations (using gsutil CLI)
```

## Quick Start

### 1. Import Types and Operations

```haskell
import Storage.Types (FileType (Image, Audio, Video, PDF), StorageConfig (..))
import qualified Storage.Flow as Storage
```

### 2. Use Storage Operations

```haskell
-- Write file
Storage.put "/path/to/file" "content"

-- Read file
content <- Storage.get "/path/to/file"

-- Delete file
Storage.delete "/path/to/file"

-- Create file path
filePath <- Storage.createFilePath "/domain/" "identifier" Image ".png"
```

### 3. Type Constraints

All Storage operations require:
```haskell
HasField "storageConfig" r StorageConfig
```

## Available Operations

### Write Operations

- **`Storage.put`**: Write text content to storage
  ```haskell
  Storage.put :: String -> Text -> m ()
  ```

- **`Storage.putRaw`**: Write binary content to storage
  ```haskell
  Storage.putRaw :: String -> ByteString -> String -> m ()
  ```

**Behavior:**
- Writes to primary storage first
- If `enableMultiCloudWrite = True` and secondary is configured, writes to secondary after primary succeeds
- Throws error if primary write fails
- Logs error (but continues) if secondary write fails

### Read Operations

- **`Storage.get`**: Read text content from storage
  ```haskell
  Storage.get :: String -> m Text
  ```

- **`Storage.headRequest`**: Get file metadata
  ```haskell
  Storage.headRequest :: String -> m ObjectStatus
  ```

**Behavior:**
- Always reads from primary storage only
- Throws error if read fails

### Delete Operations

- **`Storage.delete`**: Delete file from storage
  ```haskell
  Storage.delete :: String -> m ()
  ```

**Behavior:**
- Deletes from primary storage first
- If `enableMultiCloudWrite = True` and secondary is configured, deletes from secondary after primary succeeds
- Throws error if primary deletion fails
- Logs error (but continues) if secondary deletion fails

### URL Generation

- **`Storage.generateUploadUrl`**: Generate pre-signed upload URL
  ```haskell
  Storage.generateUploadUrl :: String -> Seconds -> m Text
  ```

- **`Storage.generateDownloadUrl`**: Generate pre-signed download URL
  ```haskell
  Storage.generateDownloadUrl :: String -> Seconds -> m Text
  ```

**Behavior:**
- Always generates URLs for primary storage only

### Path Creation

- **`Storage.createFilePath`**: Create file path with timestamp
  ```haskell
  Storage.createFilePath :: Text -> Text -> FileType -> Text -> m Text
  ```
  - Uses primary storage's path prefix
  - Generates timestamp-based filename

- **`Storage.createFilePublicPath`**: Create public file path without timestamp
  ```haskell
  Storage.createFilePublicPath :: Text -> Text -> Text -> Text -> m Text
  ```
  - Uses primary storage's path prefix
  - Uses provided filename directly

## Types

All types are exported from `Storage.Types`:

```haskell
import Storage.Types
  ( StorageConfig (..),      -- Multi-cloud configuration
    StorageProvider (..),     -- S3 or GCS provider
    FileType (..),           -- Audio, Video, Image, AudioLink, VideoLink, ImageLink, PortraitVideoLink, PDF
    ObjectStatus (..),       -- File metadata (size, eTag)
    EntityTag (..),          -- Entity tag for files
    eTagToHash,              -- Convert eTag to hash
    S3Config,                -- S3 configuration
    GCSConfig                -- GCS configuration
  )
```

## Configuration

### StorageConfig

```haskell
data StorageConfig = StorageConfig
  { primaryStorage :: StorageProvider,      -- Primary storage (S3 or GCS)
    secondaryStorage :: Maybe StorageProvider,  -- Optional secondary storage
    enableMultiCloudWrite :: Bool          -- Enable writing to both clouds
  }
```

### StorageProvider

```haskell
data StorageProvider
  = StorageS3 S3Config    -- AWS S3 configuration
  | StorageGCS GCSConfig  -- Google Cloud Storage configuration
```

## Migration Guide

### From S3 to Storage

1. **Update imports:**
   ```haskell
   -- Old
   import AWS.S3.Types (FileType (..))
   import qualified AWS.S3 as S3

   -- New
   import Storage.Types (FileType (..))
   import qualified Storage.Flow as Storage
   ```

2. **Update function calls:**
   ```haskell
   -- Old
   S3.put path content
   S3.createFilePath domain identifier fileType ext

   -- New
   Storage.put path content
   Storage.createFilePath domain identifier fileType ext
   ```

3. **Update type references:**
   ```haskell
   -- Old
   S3.Image
   S3.Audio

   -- New
   Image  -- from Storage.Types
   Audio  -- from Storage.Types
   ```

4. **Update type constraints:**
   ```haskell
   -- Old (for S3 operations)
   HasField "s3Env" r (S3Env m)

   -- New (for Storage operations)
   HasField "storageConfig" r StorageConfig
   ```

## Examples

### Example 1: Upload Image

```haskell
import Storage.Types (FileType (Image))
import qualified Storage.Flow as Storage

uploadImage :: (HasField "storageConfig" r StorageConfig, MonadFlow m) => Text -> m Text
uploadImage imageContent = do
  filePath <- Storage.createFilePath "/images/" "user-123" Image ".png"
  Storage.put (T.unpack filePath) imageContent
  return filePath
```

### Example 2: Multi-Cloud Write

```haskell
-- With enableMultiCloudWrite = True and secondaryStorage configured:
-- This will write to both S3 (primary) and GCS (secondary)
Storage.put "/path/to/file" "content"
```

### Example 3: Read File

```haskell
-- Always reads from primary storage
content <- Storage.get "/path/to/file"
```

## GCS Requirements

For GCS to work:

1. **Google Cloud SDK** (`gsutil`) must be installed on the server
2. **Service Account Key** (JSON) with permissions:
   - `storage.objects.create`
   - `storage.objects.get`
   - `storage.objects.delete`
3. **GCS Bucket** created in your GCP project

## Testing

### Mock Configuration

For local testing, use mock configurations:

```dhall
let gcsMockConfig =
      GCSConfig.GCSMockConf
        { baseLocalDirectory = "./gcs/local"
        , pathPrefix = ""
        , bucketName = "test-bucket"
        }

let storageConfig =
      StorageConfig::StorageConfig
        { primaryStorage = StorageConfig.StorageGCS gcsMockConfig
        , secondaryStorage = None StorageConfig.StorageProvider
        , enableMultiCloudWrite = False
        }
```

This uses local file system instead of actual GCS.

## See Also

- `STORAGE_SETUP_GUIDE.md` - Detailed setup instructions
- `COMPLETE_SETUP.md` - Complete setup checklist
- `test/StorageTest.hs` - Test examples

