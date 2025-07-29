{-# LANGUAGE OverloadedStrings #-}

module RegistrationTest where

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

loginOtpEndpoint :: String
loginOtpEndpoint = apiBaseUrl ++ "/api/dev/bpp/driver-offer/MSIL_PARTNER/Delhi/fleet/v2/login/otp"

verifyOtpEndpoint :: String
verifyOtpEndpoint = apiBaseUrl ++ "/api/dev/bpp/driver-offer/MSIL_PARTNER/Delhi/fleet/v2/verify/otp"

-- Real test data from your collection
realMobileNumber :: String
realMobileNumber = "7811111112"

realOtp :: String
realOtp = "7891"

-- Integration tests for Login OTP API (REAL API calls)
testLoginOtp :: TestTree
testLoginOtp =
  testGroup
    "Login OTP Integration Tests"
    [ testCase "Real login OTP should succeed" $ do
        let requestBody =
              Aeson.object
                [ "mobileNumber" Aeson..= (realMobileNumber :: String),
                  "mobileCountryCode" Aeson..= ("+91" :: String),
                  "city" Aeson..= ("Delhi" :: String),
                  "merchantId" Aeson..= ("MSIL_PARTNER" :: String)
                ]

        request <- parseRequest $ "POST " ++ loginOtpEndpoint
        let request' = setRequestBodyJSON requestBody request

        response <- httpJSON request' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 200
          then do
            let responseBody = getResponseBody response :: Aeson.Value
            -- Verify successful OTP sent response
            return () -- Success
          else assertFailure $ "Expected 200, got " ++ show status,
      testCase "Real login OTP with invalid mobile should fail" $ do
        let requestBody =
              Aeson.object
                [ "mobileNumber" Aeson..= ("9999999999" :: String),
                  "mobileCountryCode" Aeson..= ("+91" :: String),
                  "city" Aeson..= ("Delhi" :: String),
                  "merchantId" Aeson..= ("MSIL_PARTNER" :: String)
                ]

        request <- parseRequest $ "POST " ++ loginOtpEndpoint
        let request' = setRequestBodyJSON requestBody request

        response <- httpJSON request' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 400 || status == 422
          then return () -- Expected failure
          else assertFailure $ "Expected 400/422 for invalid mobile, got " ++ show status
    ]

-- Integration tests for Verify OTP API (REAL API calls)
testVerifyOtp :: TestTree
testVerifyOtp =
  testGroup
    "Verify OTP Integration Tests"
    [ testCase "Real OTP verification should succeed" $ do
        let requestBody =
              Aeson.object
                [ "mobileNumber" Aeson..= (realMobileNumber :: String),
                  "mobileCountryCode" Aeson..= ("+91" :: String),
                  "otp" Aeson..= (realOtp :: String),
                  "city" Aeson..= ("Delhi" :: String),
                  "merchantId" Aeson..= ("MSIL_PARTNER" :: String)
                ]

        request <- parseRequest $ "POST " ++ verifyOtpEndpoint
        let request' = setRequestBodyJSON requestBody request

        response <- httpJSON request' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 200
          then do
            let responseBody = getResponseBody response :: Aeson.Value
            -- Verify successful verification response
            case responseBody of
              Aeson.Object obj ->
                case KeyMap.lookup "authToken" obj of
                  Just _ -> return () -- Success
                  Nothing -> assertFailure "Response missing authToken"
              _ -> assertFailure "Response is not an object"
          else assertFailure $ "Expected 200, got " ++ show status,
      testCase "Real OTP verification with invalid OTP should fail" $ do
        let requestBody =
              Aeson.object
                [ "mobileNumber" Aeson..= (realMobileNumber :: String),
                  "mobileCountryCode" Aeson..= ("+91" :: String),
                  "otp" Aeson..= ("0000" :: String),
                  "city" Aeson..= ("Delhi" :: String),
                  "merchantId" Aeson..= ("MSIL_PARTNER" :: String)
                ]

        request <- parseRequest $ "POST " ++ verifyOtpEndpoint
        let request' = setRequestBodyJSON requestBody request

        response <- httpJSON request' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 400 || status == 422
          then return () -- Expected failure
          else assertFailure $ "Expected 400/422 for invalid OTP, got " ++ show status
    ]

-- Combined test group for all registration integration tests
registrationTests :: TestTree
registrationTests =
  testGroup
    "Registration Integration Tests (Real API)"
    [ testLoginOtp,
      testVerifyOtp
    ]
