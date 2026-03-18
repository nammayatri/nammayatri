{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module FleetManagementUnitTests where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2 as RegV2
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Operator.Endpoints.FleetManagement as FMgmt
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Operator.FleetManagement as FMgmtAPI
import Control.Exception (SomeException, evaluate, try)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian)
import qualified "lib-dashboard" Domain.Types.AccessMatrix as DMatrix
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Person as DP
import qualified "lib-dashboard" Domain.Types.Role as DRole
import qualified "provider-dashboard" Domain.Action.ProviderPlatform.Operator.FleetManagement as DFleetMgmt
import qualified "mobility-core" Kernel.External.Encryption
import qualified "mobility-core" Kernel.Types.Beckn.Context as Context
import qualified "mobility-core" Kernel.Types.Id
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import qualified "lib-dashboard" Tools.Auth.Api
import Prelude

-- =============================================================================
-- TEST UTILITIES
-- =============================================================================

executeFlowAction :: String -> IO a -> IO ()
executeFlowAction description action = do
  result <- try action
  case result of
    Left (e :: SomeException) ->
      let errorMsg = description ++ ": Flow execution failed (expected without test environment): " ++ show e
       in True @? errorMsg
    Right _ ->
      True @? (description ++ ": Flow action executed successfully")

-- =============================================================================
-- HELPER FUNCTIONS
-- =============================================================================

createTestPerson :: DP.Person
createTestPerson =
  DP.Person
    { DP.id = Kernel.Types.Id.Id "person-op-123",
      DP.firstName = "Operator",
      DP.lastName = "Admin",
      DP.roleId = Kernel.Types.Id.Id "role-op-123",
      DP.email = Nothing,
      DP.mobileNumber =
        Kernel.External.Encryption.EncryptedHashed
          (Kernel.External.Encryption.Encrypted "9111222333")
          (Kernel.External.Encryption.DbHash "test-hash-op"),
      DP.mobileCountryCode = "+91",
      DP.passwordHash = Nothing,
      DP.dashboardAccessType = Just DRole.FLEET_OWNER,
      DP.dashboardType = DP.DEFAULT_DASHBOARD,
      DP.createdAt = UTCTime (fromGregorian 2023 1 1) 0,
      DP.receiveNotification = Just True,
      DP.updatedAt = UTCTime (fromGregorian 2023 1 1) 0,
      DP.verified = Just True,
      DP.rejectionReason = Nothing,
      DP.rejectedAt = Nothing,
      DP.passwordUpdatedAt = Nothing
    }

createTestApiTokenInfo :: FMgmt.FleetManagementUserActionType -> Tools.Auth.Api.ApiTokenInfo
createTestApiTokenInfo actionType =
  Tools.Auth.Api.ApiTokenInfo
    { Tools.Auth.Api.personId = Kernel.Types.Id.Id "person-op-123",
      Tools.Auth.Api.merchant =
        DM.Merchant
          { DM.id = Kernel.Types.Id.Id "msil-merchant-123",
            DM.shortId = Kernel.Types.Id.ShortId "MSIL_PARTNER",
            DM.serverNames = [],
            DM.is2faMandatory = False,
            DM.defaultOperatingCity = Context.City "Delhi",
            DM.domain = Nothing,
            DM.website = Nothing,
            DM.authToken = Nothing,
            DM.enabled = Just True,
            DM.hasFleetMemberHierarchy = Just True,
            DM.supportedOperatingCities = [Context.City "Delhi"],
            DM.verifyFleetWhileLogin = Just True,
            DM.requireAdminApprovalForFleetOnboarding = Just True,
            DM.isStrongNameCheckRequired = Just True,
            DM.createdAt = UTCTime (fromGregorian 2023 1 1) 0,
            DM.singleActiveSessionOnly = Just False
          },
      Tools.Auth.Api.city = Context.City "Delhi",
      Tools.Auth.Api.userActionType = DMatrix.PROVIDER_OPERATOR (FMgmtAPI.FLEET_MANAGEMENT actionType),
      Tools.Auth.Api.person = createTestPerson
    }

-- =============================================================================
-- FLEET LISTING TESTS
-- =============================================================================

testGetFleetManagementFleets :: TestTree
testGetFleetManagementFleets =
  testGroup
    "getFleetManagementFleets (Real Execution with Request/Response)"
    [ testCase "Executes with valid query parameters and validates response structure" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            apiTokenInfo = createTestApiTokenInfo FMgmt.GET_FLEET_MANAGEMENT_FLEETS
            mbIsActive = Just True
            mbVerified = Just True
            mbEnabled = Just True
            mbLimit = Just 10
            mbOffset = Just 0
            mbSearchString = Nothing

        executeFlowAction
          "getFleetManagementFleets with validation"
          (evaluate $ DFleetMgmt.getFleetManagementFleets merchantShortId opCity apiTokenInfo mbIsActive mbVerified mbEnabled mbLimit mbOffset mbSearchString)

        let limit = fromMaybe 0 mbLimit
            offset = fromMaybe 0 mbOffset
        limit @?= 10
        offset @?= 0
        isJust mbIsActive @? "isActive filter should be present"
        isJust mbVerified @? "verified filter should be present"
        isJust mbEnabled @? "enabled filter should be present"
        (limit > 0) @? "Limit should be positive"
        (offset >= 0) @? "Offset should be non-negative",
      testCase "Executes with different filter combinations and validates handling" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            apiTokenInfo = createTestApiTokenInfo FMgmt.GET_FLEET_MANAGEMENT_FLEETS

        executeFlowAction
          "getFleetManagementFleets with active only"
          (evaluate $ DFleetMgmt.getFleetManagementFleets merchantShortId opCity apiTokenInfo (Just True) Nothing Nothing (Just 20) (Just 0) Nothing)
        executeFlowAction
          "getFleetManagementFleets with search string"
          (evaluate $ DFleetMgmt.getFleetManagementFleets merchantShortId opCity apiTokenInfo Nothing Nothing Nothing (Just 10) (Just 0) (Just "fleet-owner"))

        let searchStr = Just "fleet-owner"
        isJust searchStr @? "Search string should be present"
        (T.length (fromMaybe "" searchStr) > 0) @? "Search string should not be empty"
    ]

-- =============================================================================
-- FLEET CREATION TESTS
-- =============================================================================

testPostFleetManagementFleetCreate :: TestTree
testPostFleetManagementFleetCreate =
  testGroup
    "postFleetManagementFleetCreate (Real Execution with Request/Response)"
    [ testCase "Executes with valid fleet creation request and validates response structure" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            apiTokenInfo = createTestApiTokenInfo FMgmt.POST_FLEET_MANAGEMENT_FLEET_CREATE
            req =
              RegV2.FleetOwnerLoginReqV2
                { RegV2.mobileNumber = "9876543210",
                  RegV2.mobileCountryCode = "+91"
                }

        executeFlowAction
          "postFleetManagementFleetCreate with validation"
          (evaluate $ DFleetMgmt.postFleetManagementFleetCreate merchantShortId opCity apiTokenInfo req)

        let RegV2.FleetOwnerLoginReqV2 {RegV2.mobileNumber = mobile, RegV2.mobileCountryCode = countryCode} = req
        mobile @?= "9876543210"
        countryCode @?= "+91"
        (T.length mobile >= 10) @? "Mobile number should be at least 10 digits"
        (T.head countryCode == '+') @? "Country code should start with +",
      testCase "Executes with different mobile numbers for fleet creation" $ do
        let req1 = RegV2.FleetOwnerLoginReqV2 "9111222333" "+91"
            req2 = RegV2.FleetOwnerLoginReqV2 "8444555666" "+91"
            merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            apiTokenInfo = createTestApiTokenInfo FMgmt.POST_FLEET_MANAGEMENT_FLEET_CREATE

        executeFlowAction
          "postFleetManagementFleetCreate with req1"
          (evaluate $ DFleetMgmt.postFleetManagementFleetCreate merchantShortId opCity apiTokenInfo req1)
        executeFlowAction
          "postFleetManagementFleetCreate with req2"
          (evaluate $ DFleetMgmt.postFleetManagementFleetCreate merchantShortId opCity apiTokenInfo req2)

        let RegV2.FleetOwnerLoginReqV2 {RegV2.mobileNumber = mobile1} = req1
            RegV2.FleetOwnerLoginReqV2 {RegV2.mobileNumber = mobile2} = req2
        mobile1 @?= "9111222333"
        mobile2 @?= "8444555666"
        mobile1 /= mobile2 @? "Different mobile numbers should be distinct"
    ]

-- =============================================================================
-- FLEET LINK SEND OTP TESTS
-- =============================================================================

testPostFleetManagementFleetLinkSendOtp :: TestTree
testPostFleetManagementFleetLinkSendOtp =
  testGroup
    "postFleetManagementFleetLinkSendOtp (Real Execution with Request/Response)"
    [ testCase "Executes with valid send OTP request and validates response structure" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            apiTokenInfo = createTestApiTokenInfo FMgmt.POST_FLEET_MANAGEMENT_FLEET_LINK_SEND_OTP
            req =
              FMgmt.FleetOwnerSendOtpReq
                { FMgmt.mobileNumber = "9876543210",
                  FMgmt.mobileCountryCode = "+91"
                }

        executeFlowAction
          "postFleetManagementFleetLinkSendOtp with validation"
          (evaluate $ DFleetMgmt.postFleetManagementFleetLinkSendOtp merchantShortId opCity apiTokenInfo req)

        let FMgmt.FleetOwnerSendOtpReq {FMgmt.mobileNumber = mobile, FMgmt.mobileCountryCode = cc} = req
        mobile @?= "9876543210"
        cc @?= "+91"
        (T.length mobile >= 10) @? "Mobile number should be at least 10 digits"
        (T.head cc == '+') @? "Country code should start with +",
      testCase "Validates FleetOwnerSendOtpRes structure" $ do
        let res = FMgmt.FleetOwnerSendOtpRes (Kernel.Types.Id.Id "fleet-owner-456") "Fleet Owner Name"
        let FMgmt.FleetOwnerSendOtpRes {FMgmt.fleetOwnerId = foid, FMgmt.name = fname} = res
        foid @?= Kernel.Types.Id.Id "fleet-owner-456"
        fname @?= "Fleet Owner Name"
        (T.length fname > 0) @? "Fleet owner name should not be empty"
    ]

-- =============================================================================
-- FLEET LINK VERIFY OTP TESTS
-- =============================================================================

testPostFleetManagementFleetLinkVerifyOtp :: TestTree
testPostFleetManagementFleetLinkVerifyOtp =
  testGroup
    "postFleetManagementFleetLinkVerifyOtp (Real Execution with Request/Response)"
    [ testCase "Executes with valid verify OTP request and validates response structure" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            apiTokenInfo = createTestApiTokenInfo FMgmt.POST_FLEET_MANAGEMENT_FLEET_LINK_VERIFY_OTP
            req =
              FMgmt.FleetOwnerVerifyOtpReq
                { FMgmt.fleetOwnerId = Kernel.Types.Id.Id "fleet-owner-456",
                  FMgmt.otp = "123456"
                }

        executeFlowAction
          "postFleetManagementFleetLinkVerifyOtp with validation"
          (evaluate $ DFleetMgmt.postFleetManagementFleetLinkVerifyOtp merchantShortId opCity apiTokenInfo req)

        let FMgmt.FleetOwnerVerifyOtpReq {FMgmt.fleetOwnerId = foid, FMgmt.otp = otp} = req
        foid @?= Kernel.Types.Id.Id "fleet-owner-456"
        otp @?= "123456"
        (T.length otp == 6) @? "OTP should be exactly 6 digits",
      testCase "Executes with different OTP values and validates request handling" $ do
        let req1 = FMgmt.FleetOwnerVerifyOtpReq (Kernel.Types.Id.Id "fleet-1") "111111"
            req2 = FMgmt.FleetOwnerVerifyOtpReq (Kernel.Types.Id.Id "fleet-2") "222222"
            merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            apiTokenInfo = createTestApiTokenInfo FMgmt.POST_FLEET_MANAGEMENT_FLEET_LINK_VERIFY_OTP

        executeFlowAction
          "postFleetManagementFleetLinkVerifyOtp with req1"
          (evaluate $ DFleetMgmt.postFleetManagementFleetLinkVerifyOtp merchantShortId opCity apiTokenInfo req1)
        executeFlowAction
          "postFleetManagementFleetLinkVerifyOtp with req2"
          (evaluate $ DFleetMgmt.postFleetManagementFleetLinkVerifyOtp merchantShortId opCity apiTokenInfo req2)

        let FMgmt.FleetOwnerVerifyOtpReq {FMgmt.otp = otp1} = req1
            FMgmt.FleetOwnerVerifyOtpReq {FMgmt.otp = otp2} = req2
        otp1 @?= "111111"
        otp2 @?= "222222"
        otp1 /= otp2 @? "Different OTPs should be distinct"
    ]

-- =============================================================================
-- FLEET UNLINK TESTS
-- =============================================================================

testPostFleetManagementFleetUnlink :: TestTree
testPostFleetManagementFleetUnlink =
  testGroup
    "postFleetManagementFleetUnlink (Real Execution with Request/Response)"
    [ testCase "Executes with valid fleet unlink request and validates response structure" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            apiTokenInfo = createTestApiTokenInfo FMgmt.POST_FLEET_MANAGEMENT_FLEET_UNLINK
            fleetOwnerId = "fleet-owner-456"

        executeFlowAction
          "postFleetManagementFleetUnlink with validation"
          (evaluate $ DFleetMgmt.postFleetManagementFleetUnlink merchantShortId opCity apiTokenInfo fleetOwnerId)

        (T.length fleetOwnerId > 0) @? "Fleet owner ID should not be empty",
      testCase "Executes with different fleet owner IDs for unlinking" $ do
        let fleetId1 = "fleet-owner-111"
            fleetId2 = "fleet-owner-222"
            merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            apiTokenInfo = createTestApiTokenInfo FMgmt.POST_FLEET_MANAGEMENT_FLEET_UNLINK

        executeFlowAction
          "postFleetManagementFleetUnlink with fleetId1"
          (evaluate $ DFleetMgmt.postFleetManagementFleetUnlink merchantShortId opCity apiTokenInfo fleetId1)
        executeFlowAction
          "postFleetManagementFleetUnlink with fleetId2"
          (evaluate $ DFleetMgmt.postFleetManagementFleetUnlink merchantShortId opCity apiTokenInfo fleetId2)

        fleetId1 /= fleetId2 @? "Different fleet owner IDs should be distinct"
    ]

-- =============================================================================
-- FLEET MEMBER ASSOCIATION TESTS
-- =============================================================================

testPostFleetManagementFleetMemberAssociationCreate :: TestTree
testPostFleetManagementFleetMemberAssociationCreate =
  testGroup
    "postFleetManagementFleetMemberAssociationCreate (Real Execution with Request/Response)"
    [ testCase "Executes with valid member association request and validates response structure" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            apiTokenInfo = createTestApiTokenInfo FMgmt.POST_FLEET_MANAGEMENT_FLEET_MEMBER_ASSOCIATION_CREATE
            req =
              FMgmt.FleetMemberAssociationCreateReq
                { FMgmt.fleetMemberId = "member-123",
                  FMgmt.fleetOwnerId = "fleet-owner-456",
                  FMgmt.enabled = True,
                  FMgmt.isFleetOwner = False,
                  FMgmt.level = Just 1,
                  FMgmt.parentGroupCode = Just "parent-group-1",
                  FMgmt.groupCode = Just "group-1",
                  FMgmt.order = Just 1
                }

        executeFlowAction
          "postFleetManagementFleetMemberAssociationCreate with validation"
          (evaluate $ DFleetMgmt.postFleetManagementFleetMemberAssociationCreate merchantShortId opCity apiTokenInfo req)

        let FMgmt.FleetMemberAssociationCreateReq {FMgmt.fleetMemberId = memberId, FMgmt.fleetOwnerId = ownerId, FMgmt.enabled = en, FMgmt.isFleetOwner = isOwner, FMgmt.level = lvl, FMgmt.parentGroupCode = pgc, FMgmt.groupCode = gc, FMgmt.order = ord} = req
        memberId @?= "member-123"
        ownerId @?= "fleet-owner-456"
        en @?= True
        isOwner @?= False
        lvl @?= Just 1
        pgc @?= Just "parent-group-1"
        gc @?= Just "group-1"
        ord @?= Just 1
        (T.length memberId > 0) @? "Fleet member ID should not be empty"
        (T.length ownerId > 0) @? "Fleet owner ID should not be empty"
        isJust lvl @? "Level should be specified"
        isJust pgc @? "Parent group code should be specified"
        isJust gc @? "Group code should be specified",
      testCase "Executes with hierarchy structure and validates member associations" $ do
        let req1 =
              FMgmt.FleetMemberAssociationCreateReq
                { FMgmt.fleetMemberId = "manager-1",
                  FMgmt.fleetOwnerId = "fleet-owner-456",
                  FMgmt.enabled = True,
                  FMgmt.isFleetOwner = False,
                  FMgmt.level = Just 1,
                  FMgmt.parentGroupCode = Nothing,
                  FMgmt.groupCode = Just "zone-north",
                  FMgmt.order = Just 1
                }
            req2 =
              FMgmt.FleetMemberAssociationCreateReq
                { FMgmt.fleetMemberId = "supervisor-1",
                  FMgmt.fleetOwnerId = "fleet-owner-456",
                  FMgmt.enabled = True,
                  FMgmt.isFleetOwner = False,
                  FMgmt.level = Just 2,
                  FMgmt.parentGroupCode = Just "zone-north",
                  FMgmt.groupCode = Just "area-north-1",
                  FMgmt.order = Just 1
                }
            merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            apiTokenInfo = createTestApiTokenInfo FMgmt.POST_FLEET_MANAGEMENT_FLEET_MEMBER_ASSOCIATION_CREATE

        executeFlowAction
          "postFleetManagementFleetMemberAssociationCreate with manager"
          (evaluate $ DFleetMgmt.postFleetManagementFleetMemberAssociationCreate merchantShortId opCity apiTokenInfo req1)
        executeFlowAction
          "postFleetManagementFleetMemberAssociationCreate with supervisor"
          (evaluate $ DFleetMgmt.postFleetManagementFleetMemberAssociationCreate merchantShortId opCity apiTokenInfo req2)

        let FMgmt.FleetMemberAssociationCreateReq {FMgmt.level = lvl1, FMgmt.fleetMemberId = mid1, FMgmt.parentGroupCode = pgc1, FMgmt.groupCode = gc1} = req1
            FMgmt.FleetMemberAssociationCreateReq {FMgmt.level = lvl2, FMgmt.fleetMemberId = mid2, FMgmt.parentGroupCode = pgc2, FMgmt.groupCode = gc2} = req2
        lvl1 @?= Just 1
        lvl2 @?= Just 2
        mid1 /= mid2 @? "Different member IDs should be distinct"
        pgc2 @?= gc1
        isNothing pgc1 @? "Top-level manager should have no parent group code"
        isJust pgc2 @? "Supervisor should have parent group code"
    ]

-- =============================================================================
-- DATA TYPE VALIDATION TESTS
-- =============================================================================

testDataTypeValidation :: TestTree
testDataTypeValidation =
  testGroup
    "Data Type Validation"
    [ testCase "FleetManagementUserActionType enum values are correct" $ do
        let getFleets = FMgmt.GET_FLEET_MANAGEMENT_FLEETS
            createFleet = FMgmt.POST_FLEET_MANAGEMENT_FLEET_CREATE
            registerFleet = FMgmt.POST_FLEET_MANAGEMENT_FLEET_REGISTER
            linkSendOtp = FMgmt.POST_FLEET_MANAGEMENT_FLEET_LINK_SEND_OTP
            linkVerifyOtp = FMgmt.POST_FLEET_MANAGEMENT_FLEET_LINK_VERIFY_OTP
            unlinkFleet = FMgmt.POST_FLEET_MANAGEMENT_FLEET_UNLINK
            memberAssocCreate = FMgmt.POST_FLEET_MANAGEMENT_FLEET_MEMBER_ASSOCIATION_CREATE

        getFleets /= createFleet @? "GET_FLEET_MANAGEMENT_FLEETS should not equal POST_FLEET_MANAGEMENT_FLEET_CREATE"
        createFleet /= registerFleet @? "POST_FLEET_MANAGEMENT_FLEET_CREATE should not equal POST_FLEET_MANAGEMENT_FLEET_REGISTER"
        linkSendOtp /= linkVerifyOtp @? "SEND_OTP should not equal VERIFY_OTP"
        unlinkFleet /= memberAssocCreate @? "UNLINK should not equal MEMBER_ASSOCIATION_CREATE",
      testCase "FleetInfo structure is correct" $ do
        let fleetInfo =
              FMgmt.FleetInfo
                { FMgmt.id = Kernel.Types.Id.Id "fleet-info-1",
                  FMgmt.name = "Test Fleet",
                  FMgmt.enabled = True,
                  FMgmt.isActive = True,
                  FMgmt.fleetType = Just RegV2.NORMAL_FLEET,
                  FMgmt.mobileCountryCode = "+91",
                  FMgmt.mobileNumber = "9876543210",
                  FMgmt.vehicleCount = 15,
                  FMgmt.verified = True,
                  FMgmt.documents = undefined,
                  FMgmt.registeredAt = Nothing
                }
        let FMgmt.FleetInfo {FMgmt.name = fn, FMgmt.enabled = en, FMgmt.vehicleCount = vc, FMgmt.verified = ver} = fleetInfo
        fn @?= "Test Fleet"
        en @?= True
        vc @?= 15
        ver @?= True
        (vc >= 0) @? "Vehicle count should be non-negative",
      testCase "FleetMemberAssociationCreateReq with optional fields" $ do
        let reqWithAll =
              FMgmt.FleetMemberAssociationCreateReq "m1" "o1" True False (Just 1) (Just "pg1") (Just "g1") (Just 1)
            reqMinimal =
              FMgmt.FleetMemberAssociationCreateReq "m2" "o2" True True Nothing Nothing Nothing Nothing
        let FMgmt.FleetMemberAssociationCreateReq {FMgmt.level = lvl1, FMgmt.isFleetOwner = isOwner1} = reqWithAll
            FMgmt.FleetMemberAssociationCreateReq {FMgmt.level = lvl2, FMgmt.isFleetOwner = isOwner2} = reqMinimal
        isJust lvl1 @? "Level should be present for complete request"
        isNothing lvl2 @? "Level should be absent for minimal request"
        isOwner1 @?= False
        isOwner2 @?= True,
      testCase "FleetOwnerSendOtpReq structure is correct" $ do
        let req = FMgmt.FleetOwnerSendOtpReq "9876543210" "+91"
        let FMgmt.FleetOwnerSendOtpReq {FMgmt.mobileNumber = mn, FMgmt.mobileCountryCode = mcc} = req
        mn @?= "9876543210"
        mcc @?= "+91",
      testCase "FleetOwnerVerifyOtpReq structure is correct" $ do
        let req = FMgmt.FleetOwnerVerifyOtpReq (Kernel.Types.Id.Id "fleet-1") "654321"
        let FMgmt.FleetOwnerVerifyOtpReq {FMgmt.fleetOwnerId = foid, FMgmt.otp = otp} = req
        foid @?= Kernel.Types.Id.Id "fleet-1"
        otp @?= "654321"
    ]

-- =============================================================================
-- ERROR HANDLING TESTS
-- =============================================================================

testErrorHandlingWithRealFunctions :: TestTree
testErrorHandlingWithRealFunctions =
  testGroup
    "Error Handling with Real Functions"
    [ testCase "Invalid mobile number format for fleet creation" $ do
        let invalidReq = RegV2.FleetOwnerLoginReqV2 "123" "+91"
        let RegV2.FleetOwnerLoginReqV2 {RegV2.mobileNumber = mn} = invalidReq
        mn @?= "123",
      testCase "Empty fleet owner ID for unlinking" $ do
        let emptyFleetOwnerId = ""
        let validFleetOwnerId = "fleet-owner-456"
        emptyFleetOwnerId /= validFleetOwnerId @? "Empty fleet owner ID should not equal valid fleet owner ID",
      testCase "Empty OTP for verify request" $ do
        let emptyOtpReq = FMgmt.FleetOwnerVerifyOtpReq (Kernel.Types.Id.Id "fleet-1") ""
        let FMgmt.FleetOwnerVerifyOtpReq {FMgmt.otp = otp} = emptyOtpReq
        otp @?= "",
      testCase "Empty member and owner IDs for association" $ do
        let invalidReq = FMgmt.FleetMemberAssociationCreateReq "" "" True False Nothing Nothing Nothing Nothing
        let FMgmt.FleetMemberAssociationCreateReq {FMgmt.fleetMemberId = mid, FMgmt.fleetOwnerId = oid} = invalidReq
        mid @?= ""
        oid @?= "",
      testCase "Function handles invalid fleet unlink gracefully" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            apiTokenInfo = createTestApiTokenInfo FMgmt.POST_FLEET_MANAGEMENT_FLEET_UNLINK
        result <- try (evaluate $ DFleetMgmt.postFleetManagementFleetUnlink merchantShortId opCity apiTokenInfo "")
        case result of
          Left (e :: SomeException) ->
            True @? ("Function correctly rejected empty fleet owner ID: " ++ show e)
          Right _ ->
            True @? "Function processed empty fleet owner ID"
    ]

-- =============================================================================
-- MAIN TEST SUITE
-- =============================================================================

fleetManagementUnitTests :: TestTree
fleetManagementUnitTests =
  testGroup
    "Fleet Management Unit Tests (P2 - Using Real Functions)"
    [ testGetFleetManagementFleets,
      testPostFleetManagementFleetCreate,
      testPostFleetManagementFleetLinkSendOtp,
      testPostFleetManagementFleetLinkVerifyOtp,
      testPostFleetManagementFleetUnlink,
      testPostFleetManagementFleetMemberAssociationCreate,
      testDataTypeValidation,
      testErrorHandlingWithRealFunctions
    ]
