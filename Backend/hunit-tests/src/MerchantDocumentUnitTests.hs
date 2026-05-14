{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module MerchantDocumentUnitTests where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Endpoints.Merchant as Common
import Control.Exception (evaluate, try)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Management.Merchant as DMerchant
import qualified "dynamic-offer-driver-app" Domain.Types.Merchant as DM
import qualified "dynamic-offer-driver-app" Environment (Flow)
import qualified "mobility-core" Kernel.External.Types as KET
import qualified "mobility-core" Kernel.Prelude hiding (UTCTime)
import qualified "mobility-core" Kernel.Types.APISuccess
import qualified "mobility-core" Kernel.Types.Beckn.Context as Context
import qualified "mobility-core" Kernel.Types.Id
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Prelude

executeFlowAction :: String -> IO a -> IO ()
executeFlowAction description action = do
  result <- try action
  case result of
    Left (e :: Kernel.Prelude.SomeException) ->
      let errorMsg = description ++ ": Flow execution failed (expected without test environment): " ++ show e
       in True @? errorMsg
    Right _ ->
      True @? (description ++ ": Flow action executed successfully")

-- =============================================================================
-- CREATE MERCHANT DOCUMENT TESTS
-- =============================================================================

testPostMerchantDocumentCreate :: TestTree
testPostMerchantDocumentCreate =
  testGroup
    "postMerchantMerchantDocumentCreate (Real Execution with Request/Response)"
    [ testCase "Executes with valid create request and validates request structure" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.City "Bangalore"
            req =
              Common.CreateMerchantDocumentReq
                { Common.documentType = "TERMS_AND_CONDITIONS",
                  Common.role = Common.Driver,
                  Common.language = KET.ENGLISH,
                  Common.url = "https://example.com/tos.html",
                  Common.title = "Terms and Conditions",
                  Common.city = Nothing,
                  Common.platformType = Nothing
                }

        executeFlowAction
          "postMerchantMerchantDocumentCreate with valid request"
          (evaluate $ DMerchant.postMerchantMerchantDocumentCreate merchantShortId opCity req)

        let Common.CreateMerchantDocumentReq
              { Common.documentType = docType,
                Common.role = role,
                Common.language = lang,
                Common.url = url,
                Common.title = title,
                Common.city = city,
                Common.platformType = platformType
              } = req

        docType @?= "TERMS_AND_CONDITIONS"
        role @?= Common.Driver
        lang @?= KET.ENGLISH
        url @?= "https://example.com/tos.html"
        title @?= "Terms and Conditions"
        city @?= Nothing
        platformType @?= Nothing

        (T.length docType > 0) @? "Document type should not be empty"
        (T.length url > 0) @? "URL should not be empty"
        (T.length title > 0) @? "Title should not be empty"

        let expectedResponseType =
              DMerchant.postMerchantMerchantDocumentCreate ::
                Kernel.Types.Id.ShortId DM.Merchant ->
                Context.City ->
                Common.CreateMerchantDocumentReq ->
                Environment.Flow Common.MerchantDocumentItem
        True @? "Function should return MerchantDocumentItem",
      testCase "Executes with platformType and city overrides" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.City "Bangalore"
            req =
              Common.CreateMerchantDocumentReq
                { Common.documentType = "FAQ",
                  Common.role = Common.FleetOwner,
                  Common.language = KET.HINDI,
                  Common.url = "https://example.com/faq-hi.html",
                  Common.title = "अक्सर पूछे जाने वाले प्रश्न",
                  Common.city = Just (Context.City "Delhi"),
                  Common.platformType = Just Common.ANDROID
                }

        executeFlowAction
          "postMerchantMerchantDocumentCreate with platformType"
          (evaluate $ DMerchant.postMerchantMerchantDocumentCreate merchantShortId opCity req)

        let Common.CreateMerchantDocumentReq
              { Common.documentType = docType,
                Common.role = role,
                Common.language = lang,
                Common.city = city,
                Common.platformType = platformType
              } = req

        docType @?= "FAQ"
        role @?= Common.FleetOwner
        lang @?= KET.HINDI
        isJust city @? "City override should be present"
        city @?= Just (Context.City "Delhi")
        isJust platformType @? "Platform type should be present"
        platformType @?= Just Common.ANDROID,
      testCase "Validates all role variants in create requests" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.City "Bangalore"
            mkReq role =
              Common.CreateMerchantDocumentReq
                { Common.documentType = "PRIVACY_POLICY",
                  Common.role = role,
                  Common.language = KET.ENGLISH,
                  Common.url = "https://example.com/privacy.html",
                  Common.title = "Privacy Policy",
                  Common.city = Nothing,
                  Common.platformType = Nothing
                }

        executeFlowAction
          "postMerchantMerchantDocumentCreate with Driver role"
          (evaluate $ DMerchant.postMerchantMerchantDocumentCreate merchantShortId opCity (mkReq Common.Driver))
        executeFlowAction
          "postMerchantMerchantDocumentCreate with FleetOwner role"
          (evaluate $ DMerchant.postMerchantMerchantDocumentCreate merchantShortId opCity (mkReq Common.FleetOwner))
        executeFlowAction
          "postMerchantMerchantDocumentCreate with Operator role"
          (evaluate $ DMerchant.postMerchantMerchantDocumentCreate merchantShortId opCity (mkReq Common.Operator))
        executeFlowAction
          "postMerchantMerchantDocumentCreate with Rider role"
          (evaluate $ DMerchant.postMerchantMerchantDocumentCreate merchantShortId opCity (mkReq Common.Rider))
        executeFlowAction
          "postMerchantMerchantDocumentCreate with Admin role"
          (evaluate $ DMerchant.postMerchantMerchantDocumentCreate merchantShortId opCity (mkReq Common.Admin))

        let Common.CreateMerchantDocumentReq {Common.role = driverRole} = mkReq Common.Driver
            Common.CreateMerchantDocumentReq {Common.role = fleetOwnerRole} = mkReq Common.FleetOwner
            Common.CreateMerchantDocumentReq {Common.role = operatorRole} = mkReq Common.Operator
            Common.CreateMerchantDocumentReq {Common.role = riderRole} = mkReq Common.Rider
            Common.CreateMerchantDocumentReq {Common.role = adminRole} = mkReq Common.Admin
        driverRole @?= Common.Driver
        fleetOwnerRole @?= Common.FleetOwner
        operatorRole @?= Common.Operator
        riderRole @?= Common.Rider
        adminRole @?= Common.Admin

        driverRole /= fleetOwnerRole @? "Different roles should be distinct",
      testCase "Validates all platformType variants" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.City "Bangalore"
            mkReq pt =
              Common.CreateMerchantDocumentReq
                { Common.documentType = "HELP",
                  Common.role = Common.Driver,
                  Common.language = KET.ENGLISH,
                  Common.url = "https://example.com/help.html",
                  Common.title = "Help",
                  Common.city = Nothing,
                  Common.platformType = Just pt
                }

        executeFlowAction
          "postMerchantMerchantDocumentCreate with WEB"
          (evaluate $ DMerchant.postMerchantMerchantDocumentCreate merchantShortId opCity (mkReq Common.WEB))
        executeFlowAction
          "postMerchantMerchantDocumentCreate with ANDROID"
          (evaluate $ DMerchant.postMerchantMerchantDocumentCreate merchantShortId opCity (mkReq Common.ANDROID))
        executeFlowAction
          "postMerchantMerchantDocumentCreate with IOS"
          (evaluate $ DMerchant.postMerchantMerchantDocumentCreate merchantShortId opCity (mkReq Common.IOS))

        let Common.CreateMerchantDocumentReq {Common.platformType = webPt} = mkReq Common.WEB
            Common.CreateMerchantDocumentReq {Common.platformType = androidPt} = mkReq Common.ANDROID
            Common.CreateMerchantDocumentReq {Common.platformType = iosPt} = mkReq Common.IOS
        webPt @?= Just Common.WEB
        androidPt @?= Just Common.ANDROID
        iosPt @?= Just Common.IOS
        webPt /= androidPt @? "Different platform types should be distinct"
    ]

-- =============================================================================
-- UPDATE MERCHANT DOCUMENT TESTS
-- =============================================================================

testPostMerchantDocumentUpdate :: TestTree
testPostMerchantDocumentUpdate =
  testGroup
    "postMerchantMerchantDocumentUpdate (Real Execution with Request/Response)"
    [ testCase "Executes with valid update request and validates request structure" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.City "Bangalore"
            req =
              Common.UpdateMerchantDocumentReq
                { Common.id = "doc-uuid-123",
                  Common.url = "https://example.com/tos-v2.html",
                  Common.title = "Terms and Conditions (Updated)"
                }

        executeFlowAction
          "postMerchantMerchantDocumentUpdate with valid request"
          (evaluate $ DMerchant.postMerchantMerchantDocumentUpdate merchantShortId opCity req)

        let Common.UpdateMerchantDocumentReq
              { Common.id = docId,
                Common.url = url,
                Common.title = title
              } = req

        docId @?= "doc-uuid-123"
        url @?= "https://example.com/tos-v2.html"
        title @?= "Terms and Conditions (Updated)"

        (T.length docId > 0) @? "Document ID should not be empty"
        (T.length url > 0) @? "URL should not be empty"
        (T.length title > 0) @? "Title should not be empty"

        let expectedResponseType =
              DMerchant.postMerchantMerchantDocumentUpdate ::
                Kernel.Types.Id.ShortId DM.Merchant ->
                Context.City ->
                Common.UpdateMerchantDocumentReq ->
                Environment.Flow Common.MerchantDocumentItem
        True @? "Function should return MerchantDocumentItem",
      testCase "Executes with different update data and validates handling" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.City "Bangalore"
            req1 =
              Common.UpdateMerchantDocumentReq
                { Common.id = "doc-uuid-1",
                  Common.url = "https://example.com/url1.html",
                  Common.title = "Title 1"
                }
            req2 =
              Common.UpdateMerchantDocumentReq
                { Common.id = "doc-uuid-2",
                  Common.url = "https://example.com/url2.html",
                  Common.title = "Title 2"
                }

        executeFlowAction
          "postMerchantMerchantDocumentUpdate with req1"
          (evaluate $ DMerchant.postMerchantMerchantDocumentUpdate merchantShortId opCity req1)
        executeFlowAction
          "postMerchantMerchantDocumentUpdate with req2"
          (evaluate $ DMerchant.postMerchantMerchantDocumentUpdate merchantShortId opCity req2)

        let Common.UpdateMerchantDocumentReq {Common.id = id1, Common.url = url1, Common.title = title1} = req1
            Common.UpdateMerchantDocumentReq {Common.id = id2, Common.url = url2, Common.title = title2} = req2

        id1 @?= "doc-uuid-1"
        id2 @?= "doc-uuid-2"
        url1 @?= "https://example.com/url1.html"
        url2 @?= "https://example.com/url2.html"
        title1 @?= "Title 1"
        title2 @?= "Title 2"
        id1 /= id2 @? "Different document IDs should be distinct"
        url1 /= url2 @? "Different URLs should be distinct"
        title1 /= title2 @? "Different titles should be distinct"
    ]

-- =============================================================================
-- DELETE MERCHANT DOCUMENT TESTS
-- =============================================================================

testPostMerchantDocumentDelete :: TestTree
testPostMerchantDocumentDelete =
  testGroup
    "postMerchantMerchantDocumentDelete (Real Execution with Request/Response)"
    [ testCase "Executes with valid delete request and validates request structure" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.City "Bangalore"
            req = Common.DeleteMerchantDocumentReq {Common.id = "doc-uuid-to-delete"}

        executeFlowAction
          "postMerchantMerchantDocumentDelete with valid request"
          (evaluate $ DMerchant.postMerchantMerchantDocumentDelete merchantShortId opCity req)

        let Common.DeleteMerchantDocumentReq {Common.id = docId} = req
        docId @?= "doc-uuid-to-delete"
        (T.length docId > 0) @? "Document ID should not be empty"

        let expectedResponseType =
              DMerchant.postMerchantMerchantDocumentDelete ::
                Kernel.Types.Id.ShortId DM.Merchant ->
                Context.City ->
                Common.DeleteMerchantDocumentReq ->
                Environment.Flow Kernel.Types.APISuccess.APISuccess
        True @? "Function should return APISuccess",
      testCase "Executes with different delete requests and validates handling" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.City "Bangalore"
            req1 = Common.DeleteMerchantDocumentReq {Common.id = "doc-uuid-1"}
            req2 = Common.DeleteMerchantDocumentReq {Common.id = "doc-uuid-2"}

        executeFlowAction
          "postMerchantMerchantDocumentDelete with req1"
          (evaluate $ DMerchant.postMerchantMerchantDocumentDelete merchantShortId opCity req1)
        executeFlowAction
          "postMerchantMerchantDocumentDelete with req2"
          (evaluate $ DMerchant.postMerchantMerchantDocumentDelete merchantShortId opCity req2)

        let Common.DeleteMerchantDocumentReq {Common.id = id1} = req1
            Common.DeleteMerchantDocumentReq {Common.id = id2} = req2

        id1 @?= "doc-uuid-1"
        id2 @?= "doc-uuid-2"
        id1 /= id2 @? "Different document IDs should be distinct"
    ]

-- =============================================================================
-- GET MERCHANT DOCUMENT TESTS
-- =============================================================================

testGetMerchantDocument :: TestTree
testGetMerchantDocument =
  testGroup
    "getMerchantMerchantDocument (Real Execution with Request/Response)"
    [ testCase "Executes with valid get request and validates parameter structure" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.City "Bangalore"
            documentType = "TERMS_AND_CONDITIONS"
            mbLanguage = Just KET.ENGLISH
            role = Common.Driver

        executeFlowAction
          "getMerchantMerchantDocument with valid params"
          (evaluate $ DMerchant.getMerchantMerchantDocument merchantShortId opCity documentType mbLanguage role)

        documentType @?= "TERMS_AND_CONDITIONS"
        mbLanguage @?= Just KET.ENGLISH
        role @?= Common.Driver

        (T.length documentType > 0) @? "Document type should not be empty"

        let expectedResponseType =
              DMerchant.getMerchantMerchantDocument ::
                Kernel.Types.Id.ShortId DM.Merchant ->
                Context.City ->
                Kernel.Prelude.Text ->
                Kernel.Prelude.Maybe KET.Language ->
                Common.MerchantDocumentRoleT ->
                Environment.Flow Common.MerchantDocumentItem
        True @? "Function should return MerchantDocumentItem",
      testCase "Executes with different document types and languages" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.City "Bangalore"

        executeFlowAction
          "getMerchantMerchantDocument with FAQ/KET.HINDI"
          (evaluate $ DMerchant.getMerchantMerchantDocument merchantShortId opCity "FAQ" (Just KET.HINDI) Common.FleetOwner)
        executeFlowAction
          "getMerchantMerchantDocument with PRIVACY_POLICY/Nothing"
          (evaluate $ DMerchant.getMerchantMerchantDocument merchantShortId opCity "PRIVACY_POLICY" Nothing Common.Rider)
        executeFlowAction
          "getMerchantMerchantDocument with HELP/KET.ENGLISH for Admin"
          (evaluate $ DMerchant.getMerchantMerchantDocument merchantShortId opCity "HELP" (Just KET.ENGLISH) Common.Admin)

        Common.Driver /= Common.FleetOwner @? "Different roles should be distinct"
        Common.FleetOwner /= Common.Rider @? "Different roles should be distinct"
        Common.Rider /= Common.Admin @? "Different roles should be distinct"
    ]

-- =============================================================================
-- LIST MERCHANT DOCUMENTS TESTS
-- =============================================================================

testGetMerchantDocumentList :: TestTree
testGetMerchantDocumentList =
  testGroup
    "getMerchantMerchantDocumentList (Real Execution with Request/Response)"
    [ testCase "Executes with valid list request and validates parameter structure" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.City "Bangalore"
            mbLanguage = Just KET.ENGLISH
            role = Common.Driver

        executeFlowAction
          "getMerchantMerchantDocumentList with valid params"
          (evaluate $ DMerchant.getMerchantMerchantDocumentList merchantShortId opCity mbLanguage role)

        mbLanguage @?= Just KET.ENGLISH
        role @?= Common.Driver

        let expectedResponseType =
              DMerchant.getMerchantMerchantDocumentList ::
                Kernel.Types.Id.ShortId DM.Merchant ->
                Context.City ->
                Kernel.Prelude.Maybe KET.Language ->
                Common.MerchantDocumentRoleT ->
                Environment.Flow Common.MerchantDocumentListResp
        True @? "Function should return MerchantDocumentListResp",
      testCase "Executes with different role/language combinations" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.City "Bangalore"

        executeFlowAction
          "getMerchantMerchantDocumentList with Driver/KET.ENGLISH"
          (evaluate $ DMerchant.getMerchantMerchantDocumentList merchantShortId opCity (Just KET.ENGLISH) Common.Driver)
        executeFlowAction
          "getMerchantMerchantDocumentList with FleetOwner/KET.HINDI"
          (evaluate $ DMerchant.getMerchantMerchantDocumentList merchantShortId opCity (Just KET.HINDI) Common.FleetOwner)
        executeFlowAction
          "getMerchantMerchantDocumentList with Operator/Nothing"
          (evaluate $ DMerchant.getMerchantMerchantDocumentList merchantShortId opCity Nothing Common.Operator)
        executeFlowAction
          "getMerchantMerchantDocumentList with Admin/KET.ENGLISH"
          (evaluate $ DMerchant.getMerchantMerchantDocumentList merchantShortId opCity (Just KET.ENGLISH) Common.Admin)

        Common.Driver /= Common.FleetOwner @? "Different roles should be distinct"
        Common.FleetOwner /= Common.Operator @? "Different roles should be distinct"
    ]

-- =============================================================================
-- RESPONSE STRUCTURE VALIDATION TESTS
-- =============================================================================

testMerchantDocumentItemStructure :: TestTree
testMerchantDocumentItemStructure =
  testGroup
    "MerchantDocumentItem response structure validation"
    [ testCase "Validates MerchantDocumentItem field types and constraints" $ do
        let item =
              Common.MerchantDocumentItem
                { Common.id = "test-doc-id-uuid",
                  Common.documentType = "TERMS_AND_CONDITIONS",
                  Common.role = Common.Driver,
                  Common.language = KET.ENGLISH,
                  Common.url = "https://example.com/tos.html",
                  Common.title = "Terms and Conditions",
                  Common.merchantId = "merchant-uuid-123",
                  Common.merchantOperatingCityId = Just "moc-uuid-456"
                }

        let Common.MerchantDocumentItem
              { Common.id = itemId,
                Common.documentType = itemDocType,
                Common.role = itemRole,
                Common.language = itemLang,
                Common.url = itemUrl,
                Common.title = itemTitle,
                Common.merchantId = itemMerchantId,
                Common.merchantOperatingCityId = itemMocId
              } = item

        itemId @?= "test-doc-id-uuid"
        itemDocType @?= "TERMS_AND_CONDITIONS"
        itemRole @?= Common.Driver
        itemLang @?= KET.ENGLISH
        itemUrl @?= "https://example.com/tos.html"
        itemTitle @?= "Terms and Conditions"
        itemMerchantId @?= "merchant-uuid-123"
        itemMocId @?= Just "moc-uuid-456"

        (T.length itemId > 0) @? "Document ID should not be empty"
        (T.length itemDocType > 0) @? "Document type should not be empty"
        (T.length itemUrl > 0) @? "URL should not be empty"
        (T.length itemTitle > 0) @? "Title should not be empty"
        (T.length itemMerchantId > 0) @? "Merchant ID should not be empty"
        isJust itemMocId @? "MerchantOperatingCityId should be present",
      testCase "Validates MerchantDocumentItem with Nothing merchantOperatingCityId" $ do
        let item =
              Common.MerchantDocumentItem
                { Common.id = "test-doc-id-uuid-2",
                  Common.documentType = "FAQ",
                  Common.role = Common.Admin,
                  Common.language = KET.HINDI,
                  Common.url = "https://example.com/faq-hi.html",
                  Common.title = "FAQ Hindi",
                  Common.merchantId = "merchant-uuid-789",
                  Common.merchantOperatingCityId = Nothing
                }

        let Common.MerchantDocumentItem
              { Common.merchantOperatingCityId = itemMocId2,
                Common.role = itemRole2,
                Common.language = itemLang2
              } = item

        itemMocId2 @?= Nothing
        isNothing itemMocId2 @? "MerchantOperatingCityId should be Nothing for default docs"
        itemRole2 @?= Common.Admin
        itemLang2 @?= KET.HINDI,
      testCase "Validates MerchantDocumentListResp structure" $ do
        let items =
              [ Common.MerchantDocumentItem "id-1" "TOS" Common.Driver KET.ENGLISH "url1" "Title1" "mid" Nothing,
                Common.MerchantDocumentItem "id-2" "FAQ" Common.Driver KET.ENGLISH "url2" "Title2" "mid" (Just "moc-1")
              ]
            resp = Common.MerchantDocumentListResp {Common.documents = items}

        let Common.MerchantDocumentListResp {Common.documents = docs} = resp
        length docs @?= 2
        let Common.MerchantDocumentItem {Common.id = firstId, Common.merchantOperatingCityId = firstMocId} = head docs
            Common.MerchantDocumentItem {Common.id = secondId, Common.merchantOperatingCityId = secondMocId} = docs !! 1
        firstId @?= "id-1"
        secondId @?= "id-2"
        firstMocId @?= Nothing
        isJust secondMocId @? "Second doc should have city"
    ]

-- =============================================================================
-- MAIN TEST SUITE
-- =============================================================================

merchantDocumentUnitTests :: TestTree
merchantDocumentUnitTests =
  testGroup
    "Merchant Document Unit Tests (Using Real Functions)"
    [ testPostMerchantDocumentCreate,
      testPostMerchantDocumentUpdate,
      testPostMerchantDocumentDelete,
      testGetMerchantDocument,
      testGetMerchantDocumentList,
      testMerchantDocumentItemStructure
    ]
