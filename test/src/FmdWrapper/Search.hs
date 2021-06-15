{-# LANGUAGE OverloadedLabels #-}

module FmdWrapper.Search where

import Beckn.Types.Core.Ack
import Beckn.Utils.Example
import Common
import Control.Lens (Setter', _Just)
import Control.Lens.At
import Data.Time.Clock.POSIX (getPOSIXTime)
import EulerHS.Prelude
import ExternalAPI.Dunzo.Types
import Fmd
import FmdWrapper.Common
import FmdWrapper.Server
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant (Header, type (:>))
import Servant.Client
import Test.Hspec hiding (context, example)
import "fmd-wrapper" Types.Beckn.API.Callback
import qualified "fmd-wrapper" Types.Beckn.API.Search as Search
import "fmd-wrapper" Types.Beckn.Catalog
import "fmd-wrapper" Types.Beckn.Category
import "fmd-wrapper" Types.Beckn.Context
import "fmd-wrapper" Types.Beckn.Descriptor
import qualified "fmd-wrapper" Types.Beckn.Domain as Domain
import qualified "fmd-wrapper" Types.Beckn.Error as Error
import qualified "fmd-wrapper" Types.Beckn.Location as Location
import "fmd-wrapper" Types.Beckn.Price
import Utils

setPickupGps :: Setter' Search.SearchReq Location.GPS
setPickupGps = #message . #intent . #pickups . ix 0 . #location . #gps . _Just

setDropGps :: Setter' Search.SearchReq Location.GPS
setDropGps = #message . #intent . #drops . ix 0 . #location . #gps . _Just

gps1 :: Location.GPS
gps1 = Location.GPS "12.9729391" "77.6294794"

gps2 :: Location.GPS
gps2 = Location.GPS "12.9354504" "77.6146828"

gps3 :: Location.GPS
gps3 = Location.GPS "13.0827" "80.2707"

numberOfDunzoCategores :: Int
numberOfDunzoCategores = 6

runSearch :: ClientEnv -> Text -> Search.SearchReq -> IO (Either ClientError AckResponse)
runSearch clientEnv orgId searchReq = do
  now <- getPOSIXTime
  let signature = decodeUtf8 $ signRequest searchReq now orgId (orgId <> "-key")
  let searchAPI = Proxy :: Proxy (Header "Authorization" Text :> Search.SearchAPI)
  runClient clientEnv $ client searchAPI (Just signature) searchReq

verifyCallbackContext :: Bool -> Text -> Context -> IO ()
verifyCallbackContext expectBppUri transactionId context = do
  context.country `shouldBe` Just "IND"
  context.domain `shouldBe` Domain.FINAL_MILE_DELIVERY
  when expectBppUri $ context.bpp_uri `shouldSatisfy` isJust
  context.transaction_id `shouldBe` transactionId
  context.action `shouldBe` "on_search"
  context.message_id `shouldBe` transactionId
  context.bap_uri `shouldSatisfy` isJust
  context.domain_version `shouldBe` Just "0.8.3"
  context.core_version `shouldBe` Just "0.8.0"

verifyDunzoCatalog :: Search.OnSearchServices -> IO ()
verifyDunzoCatalog onSearchServices = do
  let catalog = Search.catalog onSearchServices
  let items_ = items catalog
  let categories_ = categories catalog
  case categories_ of
    [category] ->
      category
        `shouldBe` Category "1" Nothing (withName "single pickup single drop") []
    _ -> expectationFailure "Exactly one category expected."
  let packageCategories = package_categories catalog
  map extractIndices items_ `shouldBe` expectedItemIndices
  map extractCategoryData packageCategories `shouldBe` expectedCategoryData
  forM_ items_ $ \item -> do
    let price_ = item.price
    currency price_ `shouldBe` "INR"
    estimated_value price_ `shouldSatisfy` isJust
    item.category_id `shouldBe` Just "1"
  where
    extractCategoryData category = (category.id, category.descriptor.name)
    extractIndices item = (item.id, item.package_category_id)
    expectedItemIndices =
      take numberOfDunzoCategores $
        zip stringNumbers maybeStringNumbers
    expectedCategoryData = zip stringNumbers $ map (Just . content) dzPackageContentList
    stringNumbers = map show numbers
    maybeStringNumbers = map Just stringNumbers
    numbers = [1 ..] :: [Int]

processResults :: Text -> CallbackData -> IO [Search.OnSearchReq]
processResults transactionId callbackData = do
  callbackResults <- readTVarIO (onSearchTVar callbackData)
  let apiKeys = map apiKey callbackResults
  let searchResults = map result callbackResults

  traverse_ verifyApiKey apiKeys
  verifyContexts searchResults

  return searchResults
  where
    verifyContexts searchResults =
      forM_ searchResults \searchResult ->
        verifyCallbackContext True transactionId $ searchResult.context

dunzoLocationError ::
  Location.GPS ->
  Location.GPS ->
  (Error.Error -> Expectation) ->
  ClientEnv ->
  CallbackData ->
  IO ()
dunzoLocationError pickupGps dropGps check clientEnv callbackData =
  withNewUUID $ \transactionId -> do
    ctx <- buildContext "search" transactionId (Just fmdTestAppBaseUrl) Nothing

    let searchReq =
          buildFMDSearchReq ctx
            & setPickupGps .~ pickupGps
            & setDropGps .~ dropGps

    gatewayResp <- runSearch clientEnv "fmd-test-app" searchReq
    assertAck gatewayResp

    waitForCallback
    searchResults <- processResults transactionId callbackData

    let errorResults = filter isLeft $ map contents searchResults
    case errorResults of
      [Left err] -> check err
      _ -> expectationFailure "Exactly one error result expected."

successfulSearch :: ClientEnv -> CallbackData -> IO ()
successfulSearch clientEnv callbackData =
  withNewUUID $ \transactionId -> do
    ctx <- buildContext "search" transactionId (Just fmdTestAppBaseUrl) Nothing

    let searchReq =
          buildFMDSearchReq ctx
            & setPickupGps .~ gps1
            & setDropGps .~ gps2

    gatewayResponse <- runSearch clientEnv "fmd-test-app" searchReq
    assertAck gatewayResponse

    waitForCallback
    searchResults <- processResults transactionId callbackData

    let dunzoResults = filter isDunzoResult searchResults

    case rights (map contents dunzoResults) of
      [message] ->
        verifyDunzoCatalog message
      _ -> expectationFailure "Expected one search result from Dunzo."
  where
    isDunzoResult result =
      bpp_uri (context result) == Just fmdWrapperBaseUrl

dunzoUnserviceableLocation :: ClientEnv -> CallbackData -> IO ()
dunzoUnserviceableLocation =
  dunzoLocationError
    gps1
    example
    ( const (return ())
    )

dunzoNearByLocation :: ClientEnv -> CallbackData -> IO ()
dunzoNearByLocation =
  dunzoLocationError
    gps1
    gps1
    ( const (return ())
    )

dunzoDifferentCity :: ClientEnv -> CallbackData -> IO ()
dunzoDifferentCity =
  dunzoLocationError
    gps1
    gps3
    ( \Error.Error {..} -> do
        code `shouldBe` "FMD001"
        _type `shouldBe` Error.DOMAIN_ERROR
    )

incorrectApiKey :: ClientEnv -> CallbackData -> IO ()
incorrectApiKey clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {core_version = Nothing}

  gatewayResponse <- runSearch clientEnv "" searchReq
  verifyError 401 "UNAUTHORIZED" gatewayResponse

incorrectAction :: ClientEnv -> CallbackData -> IO ()
incorrectAction clientEnv _ = do
  ctx <- buildContext "" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx

  gatewayResponse <- runSearch clientEnv "fmd-test-app" searchReq
  verifyError 400 "INVALID_ACTION" gatewayResponse

incorrectCountry :: ClientEnv -> CallbackData -> IO ()
incorrectCountry clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {country = Just ""}

  gatewayResponse <- runSearch clientEnv "fmd-test-app" searchReq
  verifyError 400 "INVALID_COUNTRY" gatewayResponse

incorrectDomainVersion :: ClientEnv -> CallbackData -> IO ()
incorrectDomainVersion clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {domain_version = Just "0.7.0"}

  gatewayResponse <- runSearch clientEnv "fmd-test-app" searchReq
  verifyError 400 "UNSUPPORTED_DOMAIN_VERSION" gatewayResponse

incorrectCoreVersion :: ClientEnv -> CallbackData -> IO ()
incorrectCoreVersion clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {core_version = Just "0.7.0"}

  gatewayResponse <- runSearch clientEnv "fmd-test-app" searchReq
  verifyError 400 "UNSUPPORTED_CORE_VERSION" gatewayResponse

missingBapUri :: ClientEnv -> CallbackData -> IO ()
missingBapUri clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" Nothing Nothing
  let searchReq = buildFMDSearchReq ctx

  gatewayResponse <- runSearch clientEnv "fmd-test-app" searchReq
  verifyError 400 "INVALID_REQUEST" gatewayResponse

missingCountry :: ClientEnv -> CallbackData -> IO ()
missingCountry clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {country = Nothing}

  gatewayResponse <- runSearch clientEnv "fmd-test-app" searchReq
  verifyError 400 "INVALID_COUNTRY" gatewayResponse

missingDomainVersion :: ClientEnv -> CallbackData -> IO ()
missingDomainVersion clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {domain_version = Nothing}

  gatewayResponse <- runSearch clientEnv "fmd-test-app" searchReq
  verifyError 400 "UNSUPPORTED_DOMAIN_VERSION" gatewayResponse

missingCoreVersion :: ClientEnv -> CallbackData -> IO ()
missingCoreVersion clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {core_version = Nothing}

  gatewayResponse <- runSearch clientEnv "fmd-test-app" searchReq
  verifyError 400 "UNSUPPORTED_CORE_VERSION" gatewayResponse

spec :: Spec
spec = do
  around withCallbackApp $ do
    appManager <- runIO $ Client.newManager tlsManagerSettings
    let appClientEnv = mkClientEnv appManager gatewayBaseUrl
    describe "Search API" do
      it "Successful search" $ successfulSearch appClientEnv
      it "Dunzo: unserviceable location" $ dunzoUnserviceableLocation appClientEnv
      it "Dunzo: nearby location" $ dunzoNearByLocation appClientEnv
      it "Dunzo: different city" $ dunzoDifferentCity appClientEnv
      it "Incorrect API key" $ incorrectApiKey appClientEnv
      it "Incorrect action" $ incorrectAction appClientEnv
      it "Incorrect country" $ incorrectCountry appClientEnv
      it "Incorrect core version" $ incorrectCoreVersion appClientEnv
      it "Incorrect domain version" $ incorrectDomainVersion appClientEnv
      it "Missing BAP uri" $ missingBapUri appClientEnv
      it "Missing country" $ missingCountry appClientEnv
      it "Missing core version" $ missingCoreVersion appClientEnv
      it "Missing domain version" $ missingDomainVersion appClientEnv
