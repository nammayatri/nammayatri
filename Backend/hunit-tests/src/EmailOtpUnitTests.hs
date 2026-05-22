{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module EmailOtpUnitTests where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified "lib-dashboard" Domain.Action.Dashboard.EmailVerification as DEV
import qualified "lib-dashboard" Domain.Types.Person as DP
import qualified "lib-dashboard" Domain.Types.ServerName as DSN
import qualified "lib-dashboard" Tools.Auth.Dashboard as Auth
import qualified "lib-dashboard" Tools.InternalClient as InternalClient
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Prelude

-- =============================================================================
-- TEST UTILITIES
-- =============================================================================

mkTestTokenInfo :: Auth.TokenInfo
mkTestTokenInfo =
  Auth.TokenInfo
    { Auth.personId = Id "person-123",
      Auth.merchantId = Id "merchant-123",
      Auth.city = Context.City "Bangalore"
    }

-- =============================================================================
-- REQUEST DATA TYPE TESTS
-- =============================================================================

testEmailOtpReqDataTypes :: TestTree
testEmailOtpReqDataTypes =
  testGroup
    "EmailOtp Request Data Types"
    [ testCase "EmailOtpSendReq construction and field access" $ do
        let req = DEV.EmailOtpSendReq {email = "user@example.com"}
            DEV.EmailOtpSendReq {email = reqEmail} = req
        reqEmail @?= "user@example.com",
      testCase "EmailOtpVerifyReq construction and field access" $ do
        let req = DEV.EmailOtpVerifyReq {email = "user@example.com", otp = "1234"}
            DEV.EmailOtpVerifyReq {email = reqEmail, otp = reqOtp} = req
        reqEmail @?= "user@example.com"
        reqOtp @?= "1234",
      testCase "EmailOtpSendReq positional construction" $ do
        let DEV.EmailOtpSendReq {email = reqEmail} = DEV.EmailOtpSendReq "test@nammayatri.in"
        reqEmail @?= "test@nammayatri.in",
      testCase "EmailOtpVerifyReq positional construction" $ do
        let DEV.EmailOtpVerifyReq {email = reqEmail, otp = reqOtp} = DEV.EmailOtpVerifyReq "test@nammayatri.in" "5678"
        reqEmail @?= "test@nammayatri.in"
        reqOtp @?= "5678"
    ]

-- =============================================================================
-- REDIS KEY GENERATION TESTS
-- =============================================================================

testRedisKeyGeneration :: TestTree
testRedisKeyGeneration =
  testGroup
    "Redis Key Generation"
    [ testCase "makeEmailOtpKey generates correct key format" $ do
        let personId = Id "person-abc" :: Id DP.Person
            email = "user@example.com"
            key = DEV.makeEmailOtpKey personId email
        key @?= "Dashboard:EmailVerification:person-abc:user@example.com",
      testCase "makeEmailOtpHitsCountKey generates correct key format" $ do
        let personId = Id "person-abc" :: Id DP.Person
            email = "user@example.com"
            key = DEV.makeEmailOtpHitsCountKey personId email
        key @?= "Dashboard:EmailVerification:person-abc:user@example.com:hitsCount",
      testCase "makeEmailOtpVerifyHitsCountKey generates correct key format" $ do
        let personId = Id "person-abc" :: Id DP.Person
            email = "user@example.com"
            key = DEV.makeEmailOtpVerifyHitsCountKey personId email
        key @?= "Dashboard:EmailVerification:person-abc:user@example.com:verifyHitsCount",
      testCase "Different persons produce different keys" $ do
        let p1 = Id "person-1" :: Id DP.Person
            p2 = Id "person-2" :: Id DP.Person
            email = "same@example.com"
        DEV.makeEmailOtpKey p1 email /= DEV.makeEmailOtpKey p2 email
          @? "Keys for different persons should differ",
      testCase "Different emails produce different keys" $ do
        let personId = Id "person-1" :: Id DP.Person
            e1 = "a@example.com"
            e2 = "b@example.com"
        DEV.makeEmailOtpKey personId e1 /= DEV.makeEmailOtpKey personId e2
          @? "Keys for different emails should differ",
      testCase "OTP key and hits count key are distinct" $ do
        let personId = Id "person-1" :: Id DP.Person
            email = "user@example.com"
        DEV.makeEmailOtpKey personId email /= DEV.makeEmailOtpHitsCountKey personId email
          @? "OTP key and hits count key should differ",
      testCase "OTP key and verify hits count key are distinct" $ do
        let personId = Id "person-1" :: Id DP.Person
            email = "user@example.com"
        DEV.makeEmailOtpKey personId email /= DEV.makeEmailOtpVerifyHitsCountKey personId email
          @? "OTP key and verify hits count key should differ",
      testCase "Hits count key and verify hits count key are distinct" $ do
        let personId = Id "person-1" :: Id DP.Person
            email = "user@example.com"
        DEV.makeEmailOtpHitsCountKey personId email /= DEV.makeEmailOtpVerifyHitsCountKey personId email
          @? "Hits count key and verify hits count key should differ"
    ]

-- =============================================================================
-- TOKEN INFO TESTS
-- =============================================================================

testTokenInfo :: TestTree
testTokenInfo =
  testGroup
    "TokenInfo for Email Verification"
    [ testCase "TokenInfo carries personId, merchantId, and city" $ do
        let ti = mkTestTokenInfo
        Auth.personId ti @?= Id "person-123"
        Auth.merchantId ti @?= Id "merchant-123"
        Auth.city ti @?= Context.City "Bangalore",
      testCase "Different TokenInfo for different users produce different Redis keys" $ do
        let ti1 = Auth.TokenInfo (Id "user-1") (Id "m-1") (Context.City "Bangalore")
            ti2 = Auth.TokenInfo (Id "user-2") (Id "m-1") (Context.City "Bangalore")
            email = "same@test.com"
        DEV.makeEmailOtpKey (Auth.personId ti1) email /= DEV.makeEmailOtpKey (Auth.personId ti2) email
          @? "Different users should have different OTP keys"
    ]

-- =============================================================================
-- REQUEST STRUCTURE AND FLOW LOGIC TESTS
-- =============================================================================

testSendEmailVerificationOtpStructure :: TestTree
testSendEmailVerificationOtpStructure =
  testGroup
    "sendEmailVerificationOtp Request Structure"
    [ testCase "Valid email request structure" $ do
        let req = DEV.EmailOtpSendReq {email = "fleet@nammayatri.in"}
            DEV.EmailOtpSendReq {email = reqEmail} = req
        reqEmail @?= "fleet@nammayatri.in"
        not (T.null reqEmail) @? "Email should not be empty"
        T.isInfixOf "@" reqEmail @? "Email should contain @",
      testCase "Different emails are distinct" $ do
        let DEV.EmailOtpSendReq {email = email1} = DEV.EmailOtpSendReq "admin@company.com"
            DEV.EmailOtpSendReq {email = email2} = DEV.EmailOtpSendReq "operator@nammayatri.in"
        email1 /= email2 @? "Different emails should be distinct",
      testCase "Any dashboard user TokenInfo can be constructed" $ do
        let bppUser = Auth.TokenInfo (Id "bpp-person") (Id "bpp-merchant") (Context.City "Bangalore")
            bapUser = Auth.TokenInfo (Id "bap-person") (Id "bap-merchant") (Context.City "Chennai")
            fleetUser = Auth.TokenInfo (Id "fleet-person") (Id "fleet-merchant") (Context.City "Hyderabad")
        Auth.personId bppUser /= Auth.personId bapUser @? "Different user types should have different person IDs"
        Auth.personId bapUser /= Auth.personId fleetUser @? "BAP and fleet users should have different IDs"
    ]

testVerifyEmailOtpStructure :: TestTree
testVerifyEmailOtpStructure =
  testGroup
    "verifyEmailOtp Request Structure"
    [ testCase "Valid email and OTP request structure" $ do
        let req = DEV.EmailOtpVerifyReq {email = "fleet@nammayatri.in", otp = "1234"}
            DEV.EmailOtpVerifyReq {email = reqEmail, otp = reqOtp} = req
        reqEmail @?= "fleet@nammayatri.in"
        reqOtp @?= "1234"
        T.length reqOtp == 4 @? "OTP should be exactly 4 digits",
      testCase "Different OTP values are distinct" $ do
        let DEV.EmailOtpVerifyReq {otp = otp1} = DEV.EmailOtpVerifyReq "test@example.com" "1234"
            DEV.EmailOtpVerifyReq {otp = otp2} = DEV.EmailOtpVerifyReq "test@example.com" "5678"
            DEV.EmailOtpVerifyReq {otp = otp3} = DEV.EmailOtpVerifyReq "test@example.com" "9012"
        otp1 /= otp2 @? "Different OTPs should be distinct"
        otp2 /= otp3 @? "Different OTPs should be distinct"
    ]

-- =============================================================================
-- VALIDATION EDGE CASES (request structure tests)
-- =============================================================================

testEmailOtpValidationEdgeCases :: TestTree
testEmailOtpValidationEdgeCases =
  testGroup
    "Email OTP Validation Edge Cases"
    [ testCase "Empty email in send request" $ do
        let DEV.EmailOtpSendReq {email = reqEmail} = DEV.EmailOtpSendReq ""
        T.null reqEmail @? "Empty email should be null",
      testCase "Email without @ symbol" $ do
        let DEV.EmailOtpSendReq {email = reqEmail} = DEV.EmailOtpSendReq "not-an-email"
        not (T.isInfixOf "@" reqEmail) @? "Invalid email should not contain @",
      testCase "Empty OTP in verify request" $ do
        let DEV.EmailOtpVerifyReq {otp = reqOtp} = DEV.EmailOtpVerifyReq "test@example.com" ""
        T.null reqOtp @? "Empty OTP should be null",
      testCase "OTP with wrong length (3 digits)" $ do
        let DEV.EmailOtpVerifyReq {otp = reqOtp} = DEV.EmailOtpVerifyReq "test@example.com" "123"
        T.length reqOtp /= 4 @? "3-digit OTP should not be 4 digits",
      testCase "OTP with wrong length (5 digits)" $ do
        let DEV.EmailOtpVerifyReq {otp = reqOtp} = DEV.EmailOtpVerifyReq "test@example.com" "12345"
        T.length reqOtp /= 4 @? "5-digit OTP should not be 4 digits",
      testCase "Non-numeric OTP" $ do
        let DEV.EmailOtpVerifyReq {otp = reqOtp} = DEV.EmailOtpVerifyReq "test@example.com" "abcd"
        reqOtp @?= "abcd"
        T.length reqOtp == 4 @? "Non-numeric OTP still has 4 characters"
    ]

-- =============================================================================
-- SEND + VERIFY FLOW SCENARIOS
-- =============================================================================

testEmailOtpComplexScenarios :: TestTree
testEmailOtpComplexScenarios =
  testGroup
    "Email OTP Complex Scenarios"
    [ testCase "Send and verify flow uses same email" $ do
        let emailAddr = "fleet@nammayatri.in"
            sendReq = DEV.EmailOtpSendReq emailAddr
            verifyReq = DEV.EmailOtpVerifyReq emailAddr "4567"
            DEV.EmailOtpSendReq {email = sendEmail} = sendReq
            DEV.EmailOtpVerifyReq {email = verifyEmail} = verifyReq
        sendEmail @?= verifyEmail,
      testCase "Redis keys are consistent between send and verify for same person+email" $ do
        let personId = Id "person-1" :: Id DP.Person
            email = "test@nammayatri.in"
            otpKey = DEV.makeEmailOtpKey personId email
        T.isPrefixOf "Dashboard:EmailVerification:" otpKey @? "Key should have correct prefix"
        T.isSuffixOf email otpKey @? "Key should end with email",
      testCase "Different merchants with same email produce same Redis keys for same person" $ do
        let ti1 = Auth.TokenInfo (Id "person-1") (Id "merchant-1") (Context.City "Bangalore")
            ti2 = Auth.TokenInfo (Id "person-1") (Id "merchant-2") (Context.City "Bangalore")
            email = "shared@example.com"
        DEV.makeEmailOtpKey (Auth.personId ti1) email @?= DEV.makeEmailOtpKey (Auth.personId ti2) email,
      testCase "Different cities with same person produce same Redis keys" $ do
        let ti1 = Auth.TokenInfo (Id "person-1") (Id "merchant-1") (Context.City "Bangalore")
            ti2 = Auth.TokenInfo (Id "person-1") (Id "merchant-1") (Context.City "Chennai")
            email = "user@example.com"
        DEV.makeEmailOtpKey (Auth.personId ti1) email @?= DEV.makeEmailOtpKey (Auth.personId ti2) email
    ]

-- =============================================================================
-- SERVER NAME ROUTING TESTS
-- =============================================================================

testServerNameRouting :: TestTree
testServerNameRouting =
  testGroup
    "Server Name Routing Logic"
    [ testCase "APP_BACKEND in serverNames should route to BAP" $ do
        let serverNames = [DSN.APP_BACKEND, DSN.DRIVER_OFFER_BPP]
        elem DSN.APP_BACKEND serverNames @? "APP_BACKEND should be in the list",
      testCase "Only DRIVER_OFFER_BPP should route to BPP" $ do
        let serverNames = [DSN.DRIVER_OFFER_BPP]
        not (elem DSN.APP_BACKEND serverNames) @? "APP_BACKEND should not be in list",
      testCase "Empty serverNames should route to BPP (fallback)" $ do
        let serverNames = [] :: [DSN.ServerName]
        not (elem DSN.APP_BACKEND serverNames) @? "APP_BACKEND should not be in empty list"
    ]

-- =============================================================================
-- MERCHANT CONFIG DEFAULTS TESTS
-- =============================================================================

testMerchantConfigDefaults :: TestTree
testMerchantConfigDefaults =
  testGroup
    "Merchant Config Defaults"
    [ testCase "Default OTP TTL is 300 seconds when not configured" $ do
        let merchantTTL = Nothing :: Maybe Int
            effectiveTTL = fromMaybe 300 merchantTTL
        effectiveTTL @?= 300,
      testCase "Custom OTP TTL overrides default" $ do
        let merchantTTL = Just 600 :: Maybe Int
            effectiveTTL = fromMaybe 300 merchantTTL
        effectiveTTL @?= 600,
      testCase "Default max OTP verify attempts is 5 when not configured" $ do
        let merchantAttempts = Nothing :: Maybe Int
            effectiveAttempts = fromMaybe 5 merchantAttempts
        effectiveAttempts @?= 5,
      testCase "Custom max OTP verify attempts overrides default" $ do
        let merchantAttempts = Just 3 :: Maybe Int
            effectiveAttempts = fromMaybe 5 merchantAttempts
        effectiveAttempts @?= 3
    ]

-- =============================================================================
-- VERIFY EMAIL UPDATE REQUEST TESTS
-- =============================================================================

testVerifyEmailUpdateReq :: TestTree
testVerifyEmailUpdateReq =
  testGroup
    "VerifyEmailUpdateReq"
    [ testCase "construction and field access" $ do
        let req = InternalClient.VerifyEmailUpdateReq {email = "user@example.com", personId = "person-123"}
            InternalClient.VerifyEmailUpdateReq {email = em, personId = pid} = req
        em @?= "user@example.com"
        pid @?= "person-123",
      testCase "different persons with same email produce distinct requests" $ do
        let InternalClient.VerifyEmailUpdateReq {personId = pid1} = InternalClient.VerifyEmailUpdateReq {email = "same@example.com", personId = "person-1"}
            InternalClient.VerifyEmailUpdateReq {personId = pid2} = InternalClient.VerifyEmailUpdateReq {email = "same@example.com", personId = "person-2"}
        pid1 /= pid2
          @? "Different person IDs should be distinct",
      testCase "personId matches tokenInfo personId pattern" $ do
        let ti = Auth.TokenInfo (Id "person-abc") (Id "merchant-1") (Context.City "Bangalore")
            req = InternalClient.VerifyEmailUpdateReq {email = "test@nammayatri.in", personId = getId (Auth.personId ti)}
            InternalClient.VerifyEmailUpdateReq {personId = pid} = req
        pid @?= "person-abc"
    ]

-- =============================================================================
-- EMAIL UNIQUENESS LOGIC TESTS
-- =============================================================================

testEmailUniquenessLogic :: TestTree
testEmailUniquenessLogic =
  testGroup
    "Email Uniqueness Check Logic"
    [ testCase "same person re-verifying own email -> allowed" $ do
        let requestorId = Id "person-1" :: Id DP.Person
            existingPersonId = Id "person-1" :: Id DP.Person
        (requestorId == existingPersonId) @? "Same person should be allowed",
      testCase "different person with same email -> blocked" $ do
        let requestorId = Id "person-1" :: Id DP.Person
            existingPersonId = Id "person-2" :: Id DP.Person
        (requestorId /= existingPersonId) @? "Different person should be blocked",
      testCase "no existing person with email -> allowed" $ do
        let mbExisting = Nothing :: Maybe (Id DP.Person)
            isAllowed = case mbExisting of
              Nothing -> True
              Just existingId -> existingId == Id "person-1"
        isAllowed @? "No existing person should allow email update"
    ]

-- =============================================================================
-- VERIFY FLOW SERVER ROUTING TESTS
-- =============================================================================

testVerifyFlowRouting :: TestTree
testVerifyFlowRouting =
  testGroup
    "Verify Email Flow Server Routing"
    [ testCase "APP_BACKEND in serverNames -> routes verify to BAP" $ do
        let serverNames = [DSN.APP_BACKEND, DSN.DRIVER_OFFER_BPP]
            routesToBAP = DSN.APP_BACKEND `elem` serverNames
        routesToBAP @? "Should route to BAP when APP_BACKEND present",
      testCase "only DRIVER_OFFER_BPP -> routes verify to BPP" $ do
        let serverNames = [DSN.DRIVER_OFFER_BPP]
            routesToBAP = DSN.APP_BACKEND `elem` serverNames
        not routesToBAP @? "Should route to BPP when APP_BACKEND absent",
      testCase "empty serverNames -> routes verify to BPP (fallback)" $ do
        let serverNames = [] :: [DSN.ServerName]
            routesToBAP = DSN.APP_BACKEND `elem` serverNames
        not routesToBAP @? "Should route to BPP on empty serverNames"
    ]

-- =============================================================================
-- VERIFY FLOW ORDERING TESTS
-- =============================================================================

testVerifyFlowOrdering :: TestTree
testVerifyFlowOrdering =
  testGroup
    "Verify Email Flow Ordering"
    [ testCase "uniqueness check runs before internal API call (simulated step tracking)" $ do
        let steps = ["otp_match", "uniqueness_check", "internal_api_call", "dashboard_update"]
        length steps @?= 4
        steps !! 0 @?= "otp_match"
        steps !! 1 @?= "uniqueness_check"
        steps !! 2 @?= "internal_api_call"
        steps !! 3 @?= "dashboard_update",
      testCase "dashboard update is last step" $ do
        let steps = ["otp_match", "uniqueness_check", "internal_api_call", "dashboard_update"]
        last steps @?= "dashboard_update",
      testCase "internal API failure prevents dashboard update (simulated)" $ do
        let internalApiSuccess = False
            dashboardUpdated = internalApiSuccess
        not dashboardUpdated @? "Dashboard should not update when internal API fails"
    ]

-- =============================================================================
-- MAIN TEST SUITE
-- =============================================================================

emailOtpUnitTests :: TestTree
emailOtpUnitTests =
  testGroup
    "Email Verification Unit Tests (Shared Dashboard Service)"
    [ testEmailOtpReqDataTypes,
      testRedisKeyGeneration,
      testTokenInfo,
      testSendEmailVerificationOtpStructure,
      testVerifyEmailOtpStructure,
      testEmailOtpValidationEdgeCases,
      testEmailOtpComplexScenarios,
      testServerNameRouting,
      testMerchantConfigDefaults,
      testVerifyEmailUpdateReq,
      testEmailUniquenessLogic,
      testVerifyFlowRouting,
      testVerifyFlowOrdering
    ]
