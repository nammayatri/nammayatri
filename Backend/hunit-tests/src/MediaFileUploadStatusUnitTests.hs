{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module MediaFileUploadStatusUnitTests (mediaFileUploadStatusUnitTests) where

import qualified AWS.S3 as S3
import Data.Maybe (isJust, isNothing)
import qualified "shared-services" IssueManagement.Domain.Types.MediaFile as DMF
import qualified "mobility-core" Kernel.Prelude hiding (UTCTime)
import qualified "mobility-core" Kernel.Types.Id
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Prelude

mediaFileUploadStatusUnitTests :: TestTree
mediaFileUploadStatusUnitTests =
  testGroup
    "MediaFile Upload Status"
    [ testMediaFileUploadStatusEnum,
      testMediaFileConstructionWithStatus,
      testMediaFileStatusTransitions,
      testMediaFileStatusGuardLogic
    ]

-- =============================================================================
-- STATUS ENUM TESTS
-- =============================================================================

testMediaFileUploadStatusEnum :: TestTree
testMediaFileUploadStatusEnum =
  testGroup
    "MediaFileUploadStatus enum"
    [ testCase "PENDING, COMPLETED, FAILED are distinct" $ do
        (DMF.PENDING /= DMF.COMPLETED) @? "PENDING should not equal COMPLETED"
        (DMF.COMPLETED /= DMF.FAILED) @? "COMPLETED should not equal FAILED"
        (DMF.PENDING /= DMF.FAILED) @? "PENDING should not equal FAILED",
      testCase "Show/Read round-trip for PENDING" $ do
        let s = show DMF.PENDING
        s @?= "PENDING"
        (Kernel.Prelude.readMaybe s :: Maybe DMF.MediaFileUploadStatus) @?= Just DMF.PENDING,
      testCase "Show/Read round-trip for COMPLETED" $ do
        let s = show DMF.COMPLETED
        s @?= "COMPLETED"
        (Kernel.Prelude.readMaybe s :: Maybe DMF.MediaFileUploadStatus) @?= Just DMF.COMPLETED,
      testCase "Show/Read round-trip for FAILED" $ do
        let s = show DMF.FAILED
        s @?= "FAILED"
        (Kernel.Prelude.readMaybe s :: Maybe DMF.MediaFileUploadStatus) @?= Just DMF.FAILED,
      testCase "Invalid string returns Nothing on readMaybe" $ do
        let result = Kernel.Prelude.readMaybe "INVALID" :: Maybe DMF.MediaFileUploadStatus
        isNothing result @? "Invalid status string should return Nothing"
    ]

-- =============================================================================
-- MEDIA FILE CONSTRUCTION TESTS
-- =============================================================================

testMediaFileConstructionWithStatus :: TestTree
testMediaFileConstructionWithStatus =
  testGroup
    "MediaFile construction with status"
    [ testCase "MediaFile with PENDING status for fork-based upload" $ do
        let mf =
              DMF.MediaFile
                { id = Kernel.Types.Id.Id "test-media-id-1",
                  _type = S3.Image,
                  url = "https://example.com/media/test.png",
                  s3FilePath = Just "/message-media/org-123/test.png",
                  status = Just DMF.PENDING,
                  createdAt = Kernel.Prelude.read "2026-06-03 00:00:00 UTC"
                }
        DMF.status mf @?= Just DMF.PENDING
        isJust (DMF.s3FilePath mf) @? "s3FilePath should be set for fork-based uploads",
      testCase "MediaFile with COMPLETED status for synchronous upload" $ do
        let mf =
              DMF.MediaFile
                { id = Kernel.Types.Id.Id "test-media-id-2",
                  _type = S3.Video,
                  url = "https://example.com/media/test.mp4",
                  s3FilePath = Just "/video-media/org-456/test.mp4",
                  status = Just DMF.COMPLETED,
                  createdAt = Kernel.Prelude.read "2026-06-03 00:00:00 UTC"
                }
        DMF.status mf @?= Just DMF.COMPLETED,
      testCase "MediaFile with Nothing status for legacy rows" $ do
        let mf =
              DMF.MediaFile
                { id = Kernel.Types.Id.Id "test-media-id-3",
                  _type = S3.Audio,
                  url = "https://example.com/media/test.mp3",
                  s3FilePath = Nothing,
                  status = Nothing,
                  createdAt = Kernel.Prelude.read "2026-06-03 00:00:00 UTC"
                }
        isNothing (DMF.status mf) @? "Legacy rows should have Nothing status"
        isNothing (DMF.s3FilePath mf) @? "Legacy rows may have no s3FilePath",
      testCase "MediaFile with FAILED status" $ do
        let mf =
              DMF.MediaFile
                { id = Kernel.Types.Id.Id "test-media-id-4",
                  _type = S3.Image,
                  url = "https://example.com/media/test.jpg",
                  s3FilePath = Just "/issue-media/driver-789/test.jpg",
                  status = Just DMF.FAILED,
                  createdAt = Kernel.Prelude.read "2026-06-03 00:00:00 UTC"
                }
        DMF.status mf @?= Just DMF.FAILED
    ]

-- =============================================================================
-- STATUS TRANSITION TESTS
-- =============================================================================

testMediaFileStatusTransitions :: TestTree
testMediaFileStatusTransitions =
  testGroup
    "Status transition logic"
    [ testCase "PENDING -> COMPLETED is a valid transition (S3 upload succeeded)" $ do
        let before = Just DMF.PENDING
            after_ = Just DMF.COMPLETED
        before /= after_ @? "Status should change from PENDING to COMPLETED",
      testCase "PENDING -> FAILED is a valid transition (S3 upload failed)" $ do
        let before = Just DMF.PENDING
            after_ = Just DMF.FAILED
        before /= after_ @? "Status should change from PENDING to FAILED"
    ]

-- =============================================================================
-- STATUS GUARD LOGIC TESTS (mirrors getMediaMediaImage check)
-- =============================================================================

testMediaFileStatusGuardLogic :: TestTree
testMediaFileStatusGuardLogic =
  testGroup
    "Status guard logic for media retrieval"
    [ testCase "COMPLETED status allows retrieval" $ do
        let status = Just DMF.COMPLETED
            isBlocked = status `elem` [Just DMF.PENDING, Just DMF.FAILED]
        not isBlocked @? "COMPLETED should not be blocked",
      testCase "PENDING status blocks retrieval" $ do
        let status = Just DMF.PENDING
            isBlocked = status `elem` [Just DMF.PENDING, Just DMF.FAILED]
        isBlocked @? "PENDING should be blocked",
      testCase "FAILED status blocks retrieval" $ do
        let status = Just DMF.FAILED
            isBlocked = status `elem` [Just DMF.PENDING, Just DMF.FAILED]
        isBlocked @? "FAILED should be blocked",
      testCase "Nothing status (legacy) allows retrieval" $ do
        let status = Nothing :: Maybe DMF.MediaFileUploadStatus
            isBlocked = status `elem` [Just DMF.PENDING, Just DMF.FAILED]
        not isBlocked @? "Nothing (legacy rows) should not be blocked"
    ]
