# Complete Storage Multi-Cloud Setup

This document provides a complete step-by-step guide to set up multi-cloud storage (S3 + GCS) with mock configurations for testing.

## ✅ What Has Been Set Up

### 1. Type Definitions (✅ Complete)
- `Storage.Types.hs` - Defines `StorageConfig` and `StorageProvider`
- `GCP.GCS.Types.hs` - Defines GCS configuration types
- `generic/common.dhall` - Added Dhall type definitions for Storage and GCS

### 2. Implementation (✅ Complete)
- `Storage.Flow.hs` - Unified storage wrapper supporting S3 and GCS
- `GCP.GCS.Flow.hs` - GCS operations using `gsutil` CLI
- All operations implemented: `put`, `get`, `delete`, `putRaw`, `headRequest`, `generateUploadUrl`, `generateDownloadUrl`, `createFilePath`, `createFilePublicPath`
- **Write behavior:** Sequential writes (primary first, then secondary if enabled)
- **Type exports:** All types (`FileType`, `ObjectStatus`, `EntityTag`, etc.) re-exported from `Storage.Types`

### 3. Example App Setup (✅ Complete - dynamic-offer-driver-app)
- ✅ Added `storageConfig` to `AppCfg`
- ✅ Added `storageConfig` to `AppEnv`
- ✅ Initialized in `buildAppEnv`
- ✅ Added mock GCS config to `dev/secrets/common.dhall`
- ✅ Added `storageConfig` to `dev/dynamic-offer-driver-app.dhall`
- ✅ Added `storageConfig` to `dev/common.dhall`

## 📋 Setup Checklist for Other Apps

To add Storage multi-cloud support to any app, follow these steps:

### Step 1: Update Environment.hs

#### 1.1 Add Import
```haskell
import qualified Storage.Types as StorageTypes
```

#### 1.2 Add to AppCfg
```haskell
data AppCfg = AppCfg
  { -- ... existing fields ...
    s3Config :: S3Config,
    s3PublicConfig :: S3Config,
    storageConfig :: StorageTypes.StorageConfig,  -- Add this
    -- ... other fields ...
  }
  deriving (Generic, FromDhall)
```

#### 1.3 Add to AppEnv
```haskell
data AppEnv = AppEnv
  { -- ... existing fields ...
    s3Config :: S3Config,
    s3PublicConfig :: S3Config,
    s3Env :: S3Env Flow,
    s3EnvPublic :: S3Env Flow,
    storageConfig :: StorageTypes.StorageConfig,  -- Add this
    -- ... other fields ...
  }
  deriving (Generic)
```

#### 1.4 Initialize in buildAppEnv
```haskell
buildAppEnv cfg@AppCfg {..} = do
  -- ... existing initialization ...
  let s3Env = buildS3Env cfg.s3Config
      s3EnvPublic = buildS3Env cfg.s3PublicConfig
      storageConfig = cfg.storageConfig  -- Add this
  -- ... rest of initialization ...
  return AppEnv {storageConfig = storageConfig, ..}
```

### Step 2: Update Dhall Configuration

#### 2.1 Add to app's Dhall file (e.g., `dev/rider-app.dhall`)
```dhall
let sec = ./secrets/rider-app.dhall

in  { -- ... existing config ...
    , storageConfig = sec.storageConfig
    }
```

#### 2.2 Ensure secrets file has storageConfig (already done in `dev/secrets/common.dhall`)
The mock configuration is already set up in `dev/secrets/common.dhall`:
```dhall
let mockGCSConfig = globalCommon.GCSConfig.GCSMockConf
      { baseLocalDirectory = "./gcs/local"
      , pathPrefix = ""
      , bucketName = "test-gcs-bucket"
      }

let mockStorageConfig =
      { primaryStorage = globalCommon.StorageProvider.StorageS3 mockS3Config
      , secondaryStorage = Some globalCommon.StorageProvider (globalCommon.StorageProvider.StorageGCS mockGCSConfig)
      , enableMultiCloudWrite = True
      }

in  { -- ... other secrets ...
    , storageConfig = mockStorageConfig
    }
```

### Step 3: Use in Code

```haskell
import qualified Storage.Flow as Storage
import Storage.Types (FileType (Image, Audio, Video, PDF))

-- Write to both S3 and GCS (if multi-cloud enabled)
-- Writes sequentially: primary first, then secondary
Storage.put "/path/to/file" "content"

-- Read from primary storage
content <- Storage.get "/path/to/file"

-- Delete from both (if multi-cloud enabled)
-- Deletes sequentially: primary first, then secondary
Storage.delete "/path/to/file"

-- Create file path (uses primary storage's path prefix)
filePath <- Storage.createFilePath "/domain/" "identifier" Image ".png"

-- Create public file path (uses primary storage's path prefix)
publicPath <- Storage.createFilePublicPath "domain" "identifier" "filename" ".png"
```

## 🧪 Mock Configuration Details

### Mock S3 Configuration
- **Location**: `./s3/local`
- **Bucket**: `test-bucket`
- **Path Prefix**: `""`

### Mock GCS Configuration
- **Location**: `./gcs/local`
- **Bucket**: `test-gcs-bucket`
- **Path Prefix**: `""`

### Mock Storage Configuration
- **Primary**: S3 (mock)
- **Secondary**: GCS (mock)
- **Multi-Cloud Write**: Enabled (`True`)

This means:
- All writes go to both `./s3/local` and `./gcs/local`
- All reads come from `./s3/local` (primary)
- All deletes remove from both locations

## 🧪 Testing

### Local Testing Setup

1. **Create test directories**:
```bash
mkdir -p ./s3/local
mkdir -p ./gcs/local
```

2. **Run your app** - The mock configuration will automatically use these directories.

3. **Verify files are written to both locations**:
```bash
# Check S3 mock location
ls -la ./s3/local/

# Check GCS mock location
ls -la ./gcs/local/
```

### Test Example

See `test/StorageTest.hs` for a complete example of all storage operations.

## 🔄 Migration Path

### Option 1: Gradual Migration
- Keep existing `S3.*` code unchanged
- Use `Storage.*` for new code
- Both can coexist

### Option 2: Full Migration (✅ Recommended - Already Complete)
- ✅ Replace all `S3.put` with `Storage.put`
- ✅ Replace all `S3.get` with `Storage.get`
- ✅ Replace all `S3.delete` with `Storage.delete`
- ✅ Replace all `S3.createFilePath` with `Storage.createFilePath`
- ✅ Update imports: `import Storage.Types (FileType (..))` and `import qualified Storage.Flow as Storage`
- ✅ Update type references: `S3.Image` → `Image`, `S3.Audio` → `Audio`, etc.
- ✅ Update type constraints: Add `HasField "storageConfig" r StorageConfig` where needed

## 📝 Configuration Options

### S3 Only (Primary)
```dhall
let storageConfig =
      { primaryStorage = globalCommon.StorageProvider.StorageS3 common.s3Config
      , secondaryStorage = None globalCommon.StorageProvider
      , enableMultiCloudWrite = False
      }
```

### GCS Only (Primary)
```dhall
let storageConfig =
      { primaryStorage = globalCommon.StorageProvider.StorageGCS gcsConfig
      , secondaryStorage = None globalCommon.StorageProvider
      , enableMultiCloudWrite = False
      }
```

### Multi-Cloud (S3 Primary + GCS Secondary)
```dhall
let storageConfig =
      { primaryStorage = globalCommon.StorageProvider.StorageS3 common.s3Config
      , secondaryStorage = Some globalCommon.StorageProvider (globalCommon.StorageProvider.StorageGCS gcsConfig)
      , enableMultiCloudWrite = True
      }
```

### Multi-Cloud (GCS Primary + S3 Secondary)
```dhall
let storageConfig =
      { primaryStorage = globalCommon.StorageProvider.StorageGCS gcsConfig
      , secondaryStorage = Some globalCommon.StorageProvider (globalCommon.StorageProvider.StorageS3 common.s3Config)
      , enableMultiCloudWrite = True
      }
```

## 🚀 Production Setup

For production, replace mock configurations with real ones:

### Real GCS Configuration
```dhall
let gcsConfig =
      globalCommon.GCSConfig.GCSConf
        { serviceAccountKey = sec.gcsServiceAccountKey  -- JSON key as Text
        , bucketName = "your-production-bucket"
        , projectId = "your-gcp-project-id"
        , pathPrefix = "production/"
        }
```

### Real S3 Configuration
```dhall
let s3Config =
      globalCommon.S3Config.S3AwsConf
        { accessKeyId = sec.awsAccessKeyId
        , secretAccessKey = sec.awsSecretAccessKey
        , bucketName = "your-production-bucket"
        , region = "us-east-1"
        , pathPrefix = "production/"
        }
```

## ✅ Verification Checklist

After setup, verify:

- [ ] `storageConfig` added to `AppCfg`
- [ ] `storageConfig` added to `AppEnv`
- [ ] `storageConfig` initialized in `buildAppEnv`
- [ ] `storageConfig` added to app's Dhall file
- [ ] Mock directories created (`./s3/local` and `./gcs/local`)
- [ ] App compiles without errors
- [ ] Test write operation creates files in both locations (if multi-cloud enabled)
- [ ] Test read operation reads from primary location
- [ ] Test delete operation removes from both locations (if multi-cloud enabled)

## 📚 Additional Resources

- See `STORAGE_SETUP_GUIDE.md` for detailed usage examples
- See `test/StorageTest.hs` for test examples
- See `Storage.Flow.hs` for API documentation

