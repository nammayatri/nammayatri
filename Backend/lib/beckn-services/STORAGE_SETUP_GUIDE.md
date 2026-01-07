# Storage Multi-Cloud Setup Guide

This guide explains how to initialize and use the Storage wrapper to enable multi-cloud storage (S3 + GCS) in your application.

## Overview

The Storage wrapper allows you to:
- Use S3, GCS, or both simultaneously
- Write to both clouds concurrently when configured
- Switch between storage providers via configuration
- Maintain backward compatibility with existing S3 code

## Step 1: Add StorageConfig to AppCfg

In your `Environment.hs` file, add `storageConfig` to `AppCfg`:

```haskell
import Storage.Types (StorageConfig)

data AppCfg = AppCfg
  { -- ... existing fields ...
    s3Config :: S3Config,
    s3PublicConfig :: S3Config,
    storageConfig :: StorageConfig,  -- Add this
    -- ... other fields ...
  }
  deriving (Generic, FromDhall)
```

## Step 2: Add StorageConfig to AppEnv

Add `storageConfig` to `AppEnv`:

```haskell
data AppEnv = AppEnv
  { -- ... existing fields ...
    s3Config :: S3Config,
    s3PublicConfig :: S3Config,
    s3Env :: S3Env Flow,
    s3EnvPublic :: S3Env Flow,
    storageConfig :: StorageConfig,  -- Add this
    -- ... other fields ...
  }
  deriving (Generic)
```

## Step 3: Initialize StorageConfig in buildAppEnv

In `buildAppEnv`, initialize `storageConfig` from `AppCfg`:

```haskell
buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv cfg@AppCfg {..} = do
  -- ... existing initialization ...
  let s3Env = buildS3Env cfg.s3Config
      s3EnvPublic = buildS3Env cfg.s3PublicConfig
      storageConfig = cfg.storageConfig  -- Add this
  -- ... rest of initialization ...
  return AppEnv {storageConfig = storageConfig, ..}
```

## Step 4: Configure in Dhall

### Option A: S3 Only (Primary)

In your app's Dhall config (e.g., `dev/rider-app.dhall`):

```dhall
let StorageConfig = ./types/StorageConfig.dhall

let storageConfig =
      StorageConfig::StorageConfig
        { primaryStorage = StorageConfig.StorageS3 common.s3Config
        , secondaryStorage = None StorageConfig.StorageProvider
        , enableMultiCloudWrite = False
        }

in  { -- ... existing config ...
    , storageConfig = storageConfig
    }
```

### Option B: GCS Only (Primary)

```dhall
let StorageConfig = ./types/StorageConfig.dhall
let GCSConfig = ./types/GCSConfig.dhall

let gcsConfig =
      GCSConfig.GCSConf
        { serviceAccountKey = "your-service-account-json-key"
        , bucketName = "your-gcs-bucket"
        , projectId = "your-gcp-project-id"
        , pathPrefix = "your-path-prefix"
        }

let storageConfig =
      StorageConfig::StorageConfig
        { primaryStorage = StorageConfig.StorageGCS gcsConfig
        , secondaryStorage = None StorageConfig.StorageProvider
        , enableMultiCloudWrite = False
        }

in  { -- ... existing config ...
    , storageConfig = storageConfig
    }
```

### Option C: Multi-Cloud (S3 Primary + GCS Secondary)

```dhall
let StorageConfig = ./types/StorageConfig.dhall
let GCSConfig = ./types/GCSConfig.dhall

let gcsConfig =
      GCSConfig.GCSConf
        { serviceAccountKey = "your-service-account-json-key"
        , bucketName = "your-gcs-bucket"
        , projectId = "your-gcp-project-id"
        , pathPrefix = "your-path-prefix"
        }

let storageConfig =
      StorageConfig::StorageConfig
        { primaryStorage = StorageConfig.StorageS3 common.s3Config
        , secondaryStorage = Some StorageConfig.StorageProvider (StorageConfig.StorageGCS gcsConfig)
        , enableMultiCloudWrite = True  -- Enable writing to both clouds
        }

in  { -- ... existing config ...
    , storageConfig = storageConfig
    }
```

### Option D: Multi-Cloud (GCS Primary + S3 Secondary)

```dhall
let StorageConfig = ./types/StorageConfig.dhall
let GCSConfig = ./types/GCSConfig.dhall

let gcsConfig =
      GCSConfig.GCSConf
        { serviceAccountKey = "your-service-account-json-key"
        , bucketName = "your-gcs-bucket"
        , projectId = "your-gcp-project-id"
        , pathPrefix = "your-path-prefix"
        }

let storageConfig =
      StorageConfig::StorageConfig
        { primaryStorage = StorageConfig.StorageGCS gcsConfig
        , secondaryStorage = Some StorageConfig.StorageProvider (StorageConfig.StorageS3 common.s3Config)
        , enableMultiCloudWrite = True
        }

in  { -- ... existing config ...
    , storageConfig = storageConfig
    }
```

## Step 5: Use Storage Wrapper in Code

### Migration from S3 to Storage

**Before (using S3 directly):**
```haskell
import qualified AWS.S3 as S3

uploadFile :: Flow ()
uploadFile = do
  let filePath = "/path/to/file"
      fileContent = "file content"
  S3.put filePath fileContent
```

**After (using Storage wrapper):**
```haskell
import qualified Storage.Flow as Storage

uploadFile :: Flow ()
uploadFile = do
  let filePath = "/path/to/file"
      fileContent = "file content"
  Storage.put filePath fileContent
```

### Available Operations

```haskell
import qualified Storage.Flow as Storage
import Storage.Types (FileType (..), ObjectStatus (..), EntityTag (..), eTagToHash)

-- Write text file
Storage.put :: String -> Text -> m ()

-- Write binary file
Storage.putRaw :: String -> ByteString -> String -> m ()  -- path, content, contentType

-- Read file (from primary storage)
Storage.get :: String -> m Text

-- Delete file (from both if multi-cloud enabled)
Storage.delete :: String -> m ()

-- Get file metadata
Storage.headRequest :: String -> m ObjectStatus

-- Generate pre-signed upload URL
Storage.generateUploadUrl :: String -> Seconds -> m Text

-- Generate pre-signed download URL
Storage.generateDownloadUrl :: String -> Seconds -> m Text

-- Create file path with timestamp (uses primary storage's path prefix)
Storage.createFilePath :: Text -> Text -> FileType -> Text -> m Text

-- Create public file path without timestamp (uses primary storage's path prefix)
Storage.createFilePublicPath :: Text -> Text -> Text -> Text -> m Text
```

### Types Available from Storage.Types

All storage-related types should be imported from `Storage.Types`:

```haskell
import Storage.Types
  ( StorageConfig (..),
    StorageProvider (..),
    FileType (..),        -- Audio, Video, Image, AudioLink, VideoLink, ImageLink, PortraitVideoLink, PDF
    ObjectStatus (..),
    EntityTag (..),
    eTagToHash,
    S3Config,
    GCSConfig
  )
```

**Important:** Always use `Storage.Types` for types, not `AWS.S3.Types` directly.

## How Multi-Cloud Works

### Write Operations (`put`, `putRaw`)

When `enableMultiCloudWrite = True` and `secondaryStorage` is set:
- **Writes to primary first, then to secondary sequentially**
- If primary fails, throws error and stops
- If secondary fails, logs error but operation continues (primary write succeeded)
- Operation succeeds if primary write succeeds

When `enableMultiCloudWrite = False`:
- **Writes only to primary storage**
- Throws error if primary write fails

### Read Operations (`get`, `headRequest`, `generateUploadUrl`, `generateDownloadUrl`)

- **Always reads from primary storage only**
- Throws error if primary read fails

### Delete Operations (`delete`)

When `enableMultiCloudWrite = True` and `secondaryStorage` is set:
- **Deletes from primary first, then from secondary sequentially**
- If primary fails, throws error and stops
- If secondary fails, logs error but operation continues (primary deletion succeeded)
- Operation succeeds if primary deletion succeeds

When `enableMultiCloudWrite = False`:
- **Deletes only from primary storage**
- Throws error if primary deletion fails

## Migration from S3 to Storage

### Complete Migration (Recommended)

**All application code should now use Storage instead of S3:**

1. **Replace imports:**
   ```haskell
   -- Old
   import AWS.S3.Types (FileType (..))
   import qualified AWS.S3 as S3

   -- New
   import Storage.Types (FileType (..))
   import qualified Storage.Flow as Storage
   ```

2. **Replace function calls:**
   ```haskell
   -- Old
   S3.put path content
   S3.get path
   S3.createFilePath domain identifier fileType ext

   -- New
   Storage.put path content
   Storage.get path
   Storage.createFilePath domain identifier fileType ext
   ```

3. **Replace type references:**
   ```haskell
   -- Old
   S3.Image
   S3.Audio
   S3.FileType

   -- New
   Image  -- from Storage.Types
   Audio  -- from Storage.Types
   FileType  -- from Storage.Types
   ```

4. **Update type constraints:**
   ```haskell
   -- Old
   HasField "s3Env" r (S3Env m)

   -- New (for Storage operations)
   HasField "storageConfig" r StorageConfig
   ```

### Backward Compatibility

**Note:** Legacy `S3.*` functions still exist for backward compatibility but should not be used in new code:
- `S3.*` functions use `s3Env` from environment
- `Storage.*` functions use `storageConfig` from environment
- Both can coexist, but new code should use `Storage.*`

## Example: Complete Migration

### 1. Add to AppCfg
```haskell
data AppCfg = AppCfg
  { -- ... existing fields ...
    storageConfig :: StorageConfig
  }
```

### 2. Add to AppEnv
```haskell
data AppEnv = AppEnv
  { -- ... existing fields ...
    storageConfig :: StorageConfig
  }
```

### 3. Initialize in buildAppEnv
```haskell
buildAppEnv cfg@AppCfg {..} = do
  -- ... existing code ...
  return AppEnv {storageConfig = cfg.storageConfig, ..}
```

### 4. Configure in Dhall
```dhall
let storageConfig =
      StorageConfig::StorageConfig
        { primaryStorage = StorageConfig.StorageS3 common.s3Config
        , secondaryStorage = Some StorageConfig.StorageProvider (StorageConfig.StorageGCS gcsConfig)
        , enableMultiCloudWrite = True
        }
```

### 5. Use in Code
```haskell
import qualified Storage.Flow as Storage

-- This will write to both S3 and GCS if configured
Storage.put "/path/to/file" "content"
```

## GCS Setup Requirements

For GCS to work, you need:

1. **Google Cloud SDK (`gsutil`)** installed on the server
2. **Service Account Key** (JSON) with appropriate permissions:
   - `storage.objects.create`
   - `storage.objects.get`
   - `storage.objects.delete`
3. **GCS Bucket** created in your GCP project

The service account key should be provided as a JSON string in the `serviceAccountKey` field of `GCSConfigDetails`.

## Testing

### Mock Configuration (for local testing)

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

This will use local file system instead of actual GCS, similar to S3 mock mode.

