{-# LANGUAGE OverloadedStrings #-}

module DocumentTest where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Simple
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Prelude

-- Real API endpoints (matching your Postman collection)
apiBaseUrl :: String
apiBaseUrl = "http://localhost:8018"

documentUploadEndpoint :: String
documentUploadEndpoint = apiBaseUrl ++ "/api/dev/bpp/driver-offer/MSIL_PARTNER/Delhi/driver/8d3e8140-c5bb-4489-8254-c7a7d3882985/document/upload"

aadharVerifyEndpoint :: String
aadharVerifyEndpoint = apiBaseUrl ++ "/api/dev/bpp/driver-offer/MSIL_PARTNER/Delhi/driver/8d3e8140-c5bb-4489-8254-c7a7d3882985/document/verify/aadhar"

gstVerifyEndpoint :: String
gstVerifyEndpoint = apiBaseUrl ++ "/api/dev/bpp/driver-offer/MSIL_PARTNER/Delhi/driver/8d3e8140-c5bb-4489-8254-c7a7d3882985/document/verify/gst"

panVerifyEndpoint :: String
panVerifyEndpoint = apiBaseUrl ++ "/api/dev/bpp/driver-offer/MSIL_PARTNER/Delhi/driver/8d3e8140-c5bb-4489-8254-c7a7d3882985/document/verify/pan"

-- Real test data from your collection
realToken :: String
realToken = "d209cae0-6307-442c-bd6e-b64f140a5d78"

realDriverId :: String
realDriverId = "8d3e8140-c5bb-4489-8254-c7a7d3882985"

realAadharImage :: String
realAadharImage = "/9j/4AAQSkZJRgABAQAAAQABAAD/2wCEAAYGBgYHBgcICAcKCwoLCg8ODAwODxYQERAREBYiFRkVFRkVIh4kHhweJB42KiYmKjY+NDI0PkxERExfWl98fKcBBgYGBgcGBwgIBwoLCgsKDw4MDA4PFhAREBEQFiIVGRUVGRUiHiQeH"

realGstImage :: String
realGstImage = "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAgGBgcGBQgHBwcJCQgKDBQNDAsLDBkSEw8UHRofHh0aHBwgJC4nICIsIxwcKDcpLDAxNDQ0Hyc5PTgyPC4zNDL/2wBDAQkJCQwLDBgNDRgyIRwhMjIyMjIyMjIyMjIyMjIyMjIyMjIyM"

realPanImage :: String
realPanImage = "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAgGBgcGBQgHBwcJCQgKDBQNDAsLDBkSEw8UHRofHh0aHBwgJC4nICIsIxwcKDcpLDAxNDQ0Hyc5PTgyPC4zNDL/2wBDAQkJCQwLDBgNDRgyIRwhMjIyMjIyMjIyMjIyMjIyMjIyMjIyM"

-- Integration tests for Document Upload API (REAL API calls)
testAadharDocumentUpload :: TestTree
testAadharDocumentUpload =
  testGroup
    "Aadhar Document Upload Integration Tests"
    [ testCase "Real Aadhar front image upload should succeed" $ do
        let requestBody =
              Aeson.object
                [ "imageBase64" Aeson..= (realAadharImage :: String),
                  "imageType" Aeson..= ("AadhaarCard" :: String)
                ]

        request <- parseRequest $ "POST " ++ documentUploadEndpoint
        let request' = setRequestBodyJSON requestBody request
        let request'' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack realToken) request'

        response <- httpJSON request'' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 200
          then do
            let responseBody = getResponseBody response :: Aeson.Value
            -- Verify that response contains imageId
            case responseBody of
              Aeson.Object obj ->
                case KeyMap.lookup "imageId" obj of
                  Just _ -> return () -- Success
                  Nothing -> assertFailure "Response missing imageId"
              _ -> assertFailure "Response is not an object"
          else assertFailure $ "Expected 200, got " ++ show status
    ]

testGstDocumentUpload :: TestTree
testGstDocumentUpload =
  testGroup
    "GST Document Upload Integration Tests"
    [ testCase "Real GST certificate upload should succeed" $ do
        let requestBody =
              Aeson.object
                [ "imageBase64" Aeson..= (realGstImage :: String),
                  "imageType" Aeson..= ("GSTCertificate" :: String)
                ]

        request <- parseRequest $ "POST " ++ documentUploadEndpoint
        let request' = setRequestBodyJSON requestBody request
        let request'' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack realToken) request'

        response <- httpJSON request'' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 200
          then do
            let responseBody = getResponseBody response :: Aeson.Value
            case responseBody of
              Aeson.Object obj ->
                case KeyMap.lookup "imageId" obj of
                  Just _ -> return () -- Success
                  Nothing -> assertFailure "Response missing imageId"
              _ -> assertFailure "Response is not an object"
          else assertFailure $ "Expected 200, got " ++ show status
    ]

testPanDocumentUpload :: TestTree
testPanDocumentUpload =
  testGroup
    "PAN Document Upload Integration Tests"
    [ testCase "Real PAN card upload should succeed" $ do
        let requestBody =
              Aeson.object
                [ "imageBase64" Aeson..= (realPanImage :: String),
                  "imageType" Aeson..= ("PanCard" :: String)
                ]

        request <- parseRequest $ "POST " ++ documentUploadEndpoint
        let request' = setRequestBodyJSON requestBody request
        let request'' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack realToken) request'

        response <- httpJSON request'' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 200
          then do
            let responseBody = getResponseBody response :: Aeson.Value
            case responseBody of
              Aeson.Object obj ->
                case KeyMap.lookup "imageId" obj of
                  Just _ -> return () -- Success
                  Nothing -> assertFailure "Response missing imageId"
              _ -> assertFailure "Response is not an object"
          else assertFailure $ "Expected 200, got " ++ show status
    ]

-- Integration tests for Document Verification API (REAL API calls)
testAadharVerification :: TestTree
testAadharVerification =
  testGroup
    "Aadhar Verification Integration Tests"
    [ testCase "Real Aadhar verification should succeed" $ do
        let requestBody =
              Aeson.object
                [ "identifierNumber" Aeson..= ("659589311654" :: String),
                  "imageId" Aeson..= ("d94c1416-8d2b-469b-b55e-947a80ae3327" :: String),
                  "driverId" Aeson..= (realDriverId :: String),
                  "optionalImageId" Aeson..= Aeson.Null
                ]

        request <- parseRequest $ "POST " ++ aadharVerifyEndpoint
        let request' = setRequestBodyJSON requestBody request
        let request'' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack realToken) request'

        response <- httpJSON request'' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 200
          then do
            let responseBody = getResponseBody response :: Aeson.Value
            -- Verify successful verification response
            return () -- Success
          else assertFailure $ "Expected 200, got " ++ show status
    ]

testGstVerification :: TestTree
testGstVerification =
  testGroup
    "GST Verification Integration Tests"
    [ testCase "Real GST verification should succeed" $ do
        let requestBody =
              Aeson.object
                [ "identifierNumber" Aeson..= ("09AAUFC6050A1ZI" :: String),
                  "imageId" Aeson..= ("00eb9405-051e-4422-a8f9-8fb40a066ac0" :: String),
                  "driverId" Aeson..= (realDriverId :: String),
                  "optionalImageId" Aeson..= Aeson.Null
                ]

        request <- parseRequest $ "POST " ++ gstVerifyEndpoint
        let request' = setRequestBodyJSON requestBody request
        let request'' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack realToken) request'

        response <- httpJSON request'' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 200
          then do
            let responseBody = getResponseBody response :: Aeson.Value
            return () -- Success
          else assertFailure $ "Expected 200, got " ++ show status
    ]

testPanVerification :: TestTree
testPanVerification =
  testGroup
    "PAN Verification Integration Tests"
    [ testCase "Real PAN verification should succeed" $ do
        let requestBody =
              Aeson.object
                [ "identifierNumber" Aeson..= ("FKHPP0736G" :: String),
                  "imageId" Aeson..= ("3a1e3b71-4de5-4cf7-9049-4544ab1698c6" :: String),
                  "driverId" Aeson..= (realDriverId :: String),
                  "optionalImageId" Aeson..= Aeson.Null
                ]

        request <- parseRequest $ "POST " ++ panVerifyEndpoint
        let request' = setRequestBodyJSON requestBody request
        let request'' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack realToken) request'

        response <- httpJSON request'' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 200
          then do
            let responseBody = getResponseBody response :: Aeson.Value
            return () -- Success
          else assertFailure $ "Expected 200, got " ++ show status
    ]

-- Combined test group for all document integration tests
documentTests :: TestTree
documentTests =
  testGroup
    "Document Integration Tests (Real API)"
    [ testAadharDocumentUpload,
      testGstDocumentUpload,
      testPanDocumentUpload,
      testAadharVerification,
      testGstVerification,
      testPanVerification
    ]
