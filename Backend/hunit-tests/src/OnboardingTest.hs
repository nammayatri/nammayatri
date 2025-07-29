{-# LANGUAGE OverloadedStrings #-}

module OnboardingTest where

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

referralDetailsEndpoint :: String
referralDetailsEndpoint = apiBaseUrl ++ "/api/dev/bpp/driver-offer/MSIL_PARTNER/Delhi/driver/8d3e8140-c5bb-4489-8254-c7a7d3882985/onboarding/referralDetails"

-- Real test data from your collection
realToken :: String
realToken = "d209cae0-6307-442c-bd6e-b64f140a5d78"

realOperatorCode :: String
realOperatorCode = "MSIL_PARTNER"

-- Integration tests for Onboarding Referral Details API (REAL API calls)
testReferralDetails :: TestTree
testReferralDetails =
  testGroup
    "Referral Details Integration Tests"
    [ testCase "Real referral details retrieval should succeed" $ do
        let requestBody =
              Aeson.object
                [ "operatorCode" Aeson..= (realOperatorCode :: String)
                ]

        request <- parseRequest $ "POST " ++ referralDetailsEndpoint
        let request' = setRequestBodyJSON requestBody request
        let request'' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack realToken) request'

        response <- httpJSON request'' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 200
          then do
            let responseBody = getResponseBody response :: Aeson.Value
            -- Verify that response contains expected fields
            case responseBody of
              Aeson.Object obj -> do
                case KeyMap.lookup "operatorCode" obj of
                  Just _ -> do
                    case KeyMap.lookup "referralCode" obj of
                      Just _ -> do
                        case KeyMap.lookup "isValid" obj of
                          Just _ -> return () -- Success
                          Nothing -> assertFailure "Response missing isValid"
                      Nothing -> assertFailure "Response missing referralCode"
                  Nothing -> assertFailure "Response missing operatorCode"
              _ -> assertFailure "Response is not an object"
          else assertFailure $ "Expected 200, got " ++ show status,
      testCase "Real referral details with invalid token should fail" $ do
        let requestBody =
              Aeson.object
                [ "operatorCode" Aeson..= (realOperatorCode :: String)
                ]

        request <- parseRequest $ "POST " ++ referralDetailsEndpoint
        let request' = setRequestBodyJSON requestBody request
        let request'' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack ("invalid-token" :: String)) request'

        response <- httpJSON request'' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 401 || status == 403
          then return () -- Expected failure
          else assertFailure $ "Expected 401/403 for invalid token, got " ++ show status,
      testCase "Real referral details with invalid operator code should fail" $ do
        let requestBody =
              Aeson.object
                [ "operatorCode" Aeson..= ("invalid-code" :: String)
                ]

        request <- parseRequest $ "POST " ++ referralDetailsEndpoint
        let request' = setRequestBodyJSON requestBody request
        let request'' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack realToken) request'

        response <- httpJSON request'' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 400 || status == 422
          then return () -- Expected failure
          else assertFailure $ "Expected 400/422 for invalid operator code, got " ++ show status
    ]

-- Combined test group for all onboarding integration tests
onboardingTests :: TestTree
onboardingTests =
  testGroup
    "Onboarding Integration Tests (Real API)"
    [ testReferralDetails
    ]
