{-# LANGUAGE OverloadedStrings #-}

module ProfileTest where

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

profileEndpoint :: String
profileEndpoint = apiBaseUrl ++ "/user/profile"

-- Real test data from your collection
realToken :: String
realToken = "d209cae0-6307-442c-bd6e-b64f140a5d78"

-- Integration tests for Profile API (REAL API calls)
testProfile :: TestTree
testProfile =
  testGroup
    "Profile Integration Tests"
    [ testCase "Real profile retrieval should succeed" $ do
        request <- parseRequest $ "GET " ++ profileEndpoint
        let request' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack realToken) request

        response <- httpJSON request' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 200
          then do
            let responseBody = getResponseBody response :: Aeson.Value
            -- Verify that response contains personId
            case responseBody of
              Aeson.Object obj ->
                case KeyMap.lookup "personId" obj of
                  Just _ -> return () -- Success
                  Nothing -> assertFailure "Response missing personId"
              _ -> assertFailure "Response is not an object"
          else assertFailure $ "Expected 200, got " ++ show status,
      testCase "Real profile retrieval with invalid token should fail" $ do
        request <- parseRequest $ "GET " ++ profileEndpoint
        let request' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack ("invalid-token" :: String)) request

        response <- httpJSON request' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 401 || status == 403
          then return () -- Expected failure
          else assertFailure $ "Expected 401/403 for invalid token, got " ++ show status
    ]

-- Combined test group for all profile integration tests
profileTests :: TestTree
profileTests =
  testGroup
    "Profile Integration Tests (Real API)"
    [ testProfile
    ]
