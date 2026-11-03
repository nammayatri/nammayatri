{-# LANGUAGE OverloadedStrings #-}

module FileManagementUnitTests (fileManagementUnitTests) where

import FileManagement.Common.UI.File
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude

fileManagementUnitTests :: TestTree
fileManagementUnitTests =
  testGroup
    "FileManagement Unit Tests"
    [ testDomainConfig,
      testCanReadFile
    ]

testDomainConfig :: TestTree
testDomainConfig =
  testGroup
    "domainConfig"
    [ testCase "RideChat s3Prefix has NO trailing slash (S3.createFilePath appends '/' after the empty identifier)" $ do
        let DomainConfig {s3Prefix = prefix} = domainConfig RideChat
        prefix @?= "ride-chat",
      testCase "RideChat has a fetch route segment (its media URL must resolve)" $ do
        let DomainConfig {routeSegment = route} = domainConfig RideChat
        route @?= Just "files"
    ]

testCanReadFile :: TestTree
testCanReadFile =
  testGroup
    "canReadFile"
    [ testCase "the uploader can read their own proof" $
        canReadFile "d1" "ride-evidence/d1/g1/Image/2026-10-19T10-00-00.000Z.png" @?= True,
      testCase "the uploader can read their own proof under an environment prefix" $
        canReadFile "d1" "dev/ride-evidence/d1/g1/Image/2026-10-19T10-00-00.000Z.png" @?= True,
      testCase "another person cannot read the proof" $
        canReadFile "d2" "ride-evidence/d1/g1/Image/2026-10-19T10-00-00.000Z.png" @?= False,
      testCase "a path that climbs out of the folder is refused" $
        canReadFile "d1" "ride-evidence/d1/../d2/g1/Image/x.png" @?= False,
      testCase "a path with a shell separator is refused" $
        canReadFile "d1" "ride-evidence/d1/g1/Image/x.png;ls" @?= False,
      testCase "a path with a space is refused" $
        canReadFile "d1" "ride-evidence/d1/g1/Image/x y.png" @?= False,
      testCase "a path with a command substitution is refused" $
        canReadFile "d1" "ride-evidence/d1/g1/Image/$(ls).png" @?= False,
      testCase "a path with a backtick is refused" $
        canReadFile "d1" "ride-evidence/d1/g1/Image/`ls`.png" @?= False,
      testCase "a path with a newline is refused" $
        canReadFile "d1" "ride-evidence/d1/g1/Image/x.png\nls" @?= False,
      testCase "a path with a NUL is refused" $
        canReadFile "d1" "ride-evidence/d1/g1/Image/x.png\0" @?= False,
      testCase "a path with a non-ASCII digit is refused" $
        canReadFile "d1" "ride-evidence/d1/g1/Image/\x661.png" @?= False,
      testCase "a path with an encoded separator left in it is refused" $
        canReadFile "d1" "ride-evidence/d1/g1/Image/x.png%3Bls" @?= False,
      testCase "a path outside the ride proofs folder is not restricted here" $
        canReadFile "d1" "ride-chat/someone/Image/x.png" @?= True
    ]
