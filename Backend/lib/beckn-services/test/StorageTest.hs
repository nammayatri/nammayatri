{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE NoImplicitPrelude #-}

module StorageTest where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.Types.Common (Seconds)
import Kernel.Utils.Common
import qualified Storage.Flow as Storage

-- | Example test function showing how to use Storage wrapper
-- This demonstrates multi-cloud storage operations
testStorageOperations :: Flow ()
testStorageOperations = do
  let testPath = "/test/file.txt"
      testContent = "Hello, Multi-Cloud Storage!"
      testBinaryContent = "Binary content" :: BS.ByteString
      contentType = "text/plain"
      expires = Seconds 3600

  -- Test 1: Write text file (writes to both S3 and GCS if multi-cloud enabled)
  logInfo "Testing Storage.put..."
  Storage.put testPath (T.pack testContent)
  logInfo "✓ Storage.put completed"

  -- Test 2: Write binary file
  logInfo "Testing Storage.putRaw..."
  Storage.putRaw testPath testBinaryContent contentType
  logInfo "✓ Storage.putRaw completed"

  -- Test 3: Read file (from primary storage)
  logInfo "Testing Storage.get..."
  content <- Storage.get testPath
  logInfo $ "✓ Storage.get completed. Content: " <> content

  -- Test 4: Get file metadata
  logInfo "Testing Storage.headRequest..."
  objectStatus <- Storage.headRequest testPath
  logInfo $ "✓ Storage.headRequest completed. File size: " <> show objectStatus.fileSizeInBytes

  -- Test 5: Generate pre-signed upload URL
  logInfo "Testing Storage.generateUploadUrl..."
  uploadUrl <- Storage.generateUploadUrl testPath expires
  logInfo "✓ Storage.generateUploadUrl completed"

  -- Test 6: Generate pre-signed download URL
  logInfo "Testing Storage.generateDownloadUrl..."
  downloadUrl <- Storage.generateDownloadUrl testPath expires
  logInfo "✓ Storage.generateDownloadUrl completed"

  -- Test 7: Delete file (from both if multi-cloud enabled)
  logInfo "Testing Storage.delete..."
  Storage.delete testPath
  logInfo "✓ Storage.delete completed"

  logInfo "All storage operations completed successfully!"

-- | Example showing migration from S3 to Storage
--
-- Before: Using S3 directly
--   import qualified AWS.S3 as S3
--   S3.put "/old/path" "content"
--
-- After: Using Storage wrapper (supports multi-cloud)
--   import qualified Storage.Flow as Storage
--   import Storage.Types (FileType (Image, Audio, Video, PDF))
--   -- This will write to both S3 and GCS if configured
--   Storage.put "/new/path" "content"
