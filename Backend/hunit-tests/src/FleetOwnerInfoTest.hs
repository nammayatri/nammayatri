{-# LANGUAGE OverloadedStrings #-}

module FleetOwnerInfoTest where

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

fleetOwnerInfoEndpoint :: String
fleetOwnerInfoEndpoint = apiBaseUrl ++ "/api/dev/bpp/driver-offer/MSIL_PARTNER/Delhi/driver/8d3e8140-c5bb-4489-8254-c7a7d3882985/fleetOwnerInfo"

updateFleetOwnerInfoEndpoint :: String
updateFleetOwnerInfoEndpoint = apiBaseUrl ++ "/api/dev/bpp/driver-offer/MSIL_PARTNER/Delhi/driver/8d3e8140-c5bb-4489-8254-c7a7d3882985/fleetOwnerInfo"

-- Real test data from your collection
realToken :: String
realToken = "d209cae0-6307-442c-bd6e-b64f140a5d78"

realDriverId :: String
realDriverId = "8d3e8140-c5bb-4489-8254-c7a7d3882985"

-- Integration tests for Fleet Owner Info API (REAL API calls)
testFleetOwnerInfo :: TestTree
testFleetOwnerInfo =
  testGroup
    "Fleet Owner Info Integration Tests"
    [ testCase "Real fleet owner info retrieval should succeed" $ do
        request <- parseRequest $ "GET " ++ fleetOwnerInfoEndpoint
        let request' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack realToken) request

        response <- httpJSON request' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 200
          then do
            let responseBody = getResponseBody response :: Aeson.Value
            -- Verify that response contains fleetOwnerId
            case responseBody of
              Aeson.Object obj ->
                case KeyMap.lookup "fleetOwnerId" obj of
                  Just _ -> return () -- Success
                  Nothing -> assertFailure "Response missing fleetOwnerId"
              _ -> assertFailure "Response is not an object"
          else assertFailure $ "Expected 200, got " ++ show status,
      testCase "Real fleet owner info with invalid token should fail" $ do
        request <- parseRequest $ "GET " ++ fleetOwnerInfoEndpoint
        let request' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack ("invalid-token" :: String)) request

        response <- httpJSON request' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 401 || status == 403
          then return () -- Expected failure
          else assertFailure $ "Expected 401/403 for invalid token, got " ++ show status
    ]

testUpdateFleetOwnerInfo :: TestTree
testUpdateFleetOwnerInfo =
  testGroup
    "Update Fleet Owner Info Integration Tests"
    [ testCase "Real fleet owner info update should succeed" $ do
        let requestBody =
              Aeson.object
                [ "fleetOwnerId" Aeson..= ("fleet-owner-123" :: String),
                  "fleetOwnerName" Aeson..= ("Test Fleet Owner" :: String),
                  "fleetOwnerPhoneNumber" Aeson..= ("+919876543210" :: String)
                ]

        request <- parseRequest $ "POST " ++ updateFleetOwnerInfoEndpoint
        let request' = setRequestBodyJSON requestBody request
        let request'' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack realToken) request'

        response <- httpJSON request'' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 200
          then do
            let responseBody = getResponseBody response :: Aeson.Value
            return () -- Success
          else assertFailure $ "Expected 200, got " ++ show status,
      testCase "Real fleet owner info update with invalid token should fail" $ do
        let requestBody =
              Aeson.object
                [ "fleetOwnerId" Aeson..= ("fleet-owner-123" :: String),
                  "fleetOwnerName" Aeson..= ("Test Fleet Owner" :: String),
                  "fleetOwnerPhoneNumber" Aeson..= ("+919876543210" :: String)
                ]

        request <- parseRequest $ "POST " ++ updateFleetOwnerInfoEndpoint
        let request' = setRequestBodyJSON requestBody request
        let request'' = addRequestHeader "token" (TE.encodeUtf8 $ T.pack ("invalid-token" :: String)) request'

        response <- httpJSON request'' :: IO (Response Aeson.Value)

        let status = getResponseStatusCode response
        if status == 401 || status == 403
          then return () -- Expected failure
          else assertFailure $ "Expected 401/403 for invalid token, got " ++ show status
    ]

-- Combined test group for all fleet owner info integration tests
fleetOwnerInfoTests :: TestTree
fleetOwnerInfoTests =
  testGroup
    "Fleet Owner Info Integration Tests (Real API)"
    [ testFleetOwnerInfo,
      testUpdateFleetOwnerInfo
    ]
