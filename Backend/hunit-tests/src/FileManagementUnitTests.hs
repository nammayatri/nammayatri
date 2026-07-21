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
    [ testDomainConfig
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
