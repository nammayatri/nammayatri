{-# LANGUAGE OverloadedLabels #-}

module FmdWrapper.Search where

import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Category
import Beckn.Types.Core.Context
import Beckn.Types.Core.Descriptor
import qualified Beckn.Types.Core.Domain as Domain
import qualified Beckn.Types.Core.Error as Error
import Beckn.Types.Core.Item
import qualified Beckn.Types.Core.Location as Location
import Beckn.Types.Core.Price
import qualified Beckn.Types.FMD.API.Search as Search
import Beckn.Types.FMD.Catalog
import Beckn.Utils.Common
import Common
import Control.Lens (Setter', _Just)
import Control.Lens.At
import EulerHS.Prelude
import External.Dunzo.Types
import Fmd
import FmdWrapper.Common
import FmdWrapper.Server
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec hiding (context, example)
import Utils

setPickupGps :: Setter' Search.SearchReq Location.GPS
setPickupGps = #message . #intent . #_pickups . ix 0 . #_location . #_gps . _Just

setDropGps :: Setter' Search.SearchReq Location.GPS
setDropGps = #message . #intent . #_drops . ix 0 . #_location . #_gps . _Just

gps1 :: Location.GPS
gps1 = Location.GPS "12.9729391" "77.6294794"

gps2 :: Location.GPS
gps2 = Location.GPS "12.9354504" "77.6146828"

gps3 :: Location.GPS
gps3 = Location.GPS "13.0827" "80.2707"

numberOfDunzoCategores :: Int
numberOfDunzoCategores = 6

runSearch :: ClientEnv -> Text -> Search.SearchReq -> IO (Either ClientError AckResponse)
runSearch clientEnv apiKey searchReq =
  runClient clientEnv $ client Search.searchAPI apiKey searchReq

verifyCallbackContext :: Bool -> Text -> Context -> IO ()
verifyCallbackContext expectBppUri transactionId context = do
  _country context `shouldBe` Just "IND"
  _domain context `shouldBe` Domain.FINAL_MILE_DELIVERY
  when expectBppUri $ _bpp_uri context `shouldSatisfy` isJust
  _transaction_id context `shouldBe` transactionId
  _action context `shouldBe` "on_search"
  _message_id context `shouldBe` transactionId
  _bap_uri context `shouldSatisfy` isJust
  _domain_version context `shouldBe` Just "0.8.3"
  _core_version context `shouldBe` Just "0.8.0"

verifyDunzoCatalog :: Search.OnSearchServices -> IO ()
verifyDunzoCatalog onSearchServices = do
  let catalog = Search.catalog onSearchServices
  let items = _items catalog
  let categories = _categories catalog
  case categories of
    [category] ->
      category
        `shouldBe` Category "1" Nothing (withName "single pickup single drop") []
    _ -> expectationFailure "Exactly one category expected."
  let packageCategories = _package_categories catalog
  map extractIndices items `shouldBe` expectedItemIndices
  map extractCategoryData packageCategories `shouldBe` expectedCategoryData
  forM_ items $ \item -> do
    let price = _price item
    _currency price `shouldBe` "INR"
    _estimated_value price `shouldSatisfy` isJust
    _category_id item `shouldBe` Just "1"
  where
    extractCategoryData category = (category ^. #_id, category ^. #_descriptor . #_name)
    extractIndices item = (item ^. #_id, item ^. #_package_category_id)
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
        verifyCallbackContext True transactionId $ searchResult ^. #context

dunzoLocationError ::
  Location.GPS ->
  Location.GPS ->
  Error.Error ->
  ClientEnv ->
  CallbackData ->
  IO ()
dunzoLocationError pickupGps dropGps expectedError clientEnv callbackData =
  withNewUUID $ \transactionId -> do
    ctx <- buildContext "search" transactionId (Just fmdTestAppBaseUrl) Nothing

    let searchReq =
          buildFMDSearchReq ctx
            & setPickupGps .~ pickupGps
            & setDropGps .~ dropGps

    gatewayResponse <- runSearch clientEnv "fmd-test-app-key" searchReq
    assertAck gatewayResponse

    _ <- waitForCallback (onSearchEndMVar callbackData)
    searchResults <- processResults transactionId callbackData

    let errorResults = filter isLeft $ map contents searchResults
    case errorResults of
      [Left err] -> err `shouldBe` expectedError
      _ -> expectationFailure "Exactly one error result expected."

successfulSearch :: ClientEnv -> CallbackData -> IO ()
successfulSearch clientEnv callbackData =
  withNewUUID $ \transactionId -> do
    ctx <- buildContext "search" transactionId (Just fmdTestAppBaseUrl) Nothing

    let searchReq =
          buildFMDSearchReq ctx
            & setPickupGps .~ gps1
            & setDropGps .~ gps2

    gatewayResponse <- runSearch clientEnv "fmd-test-app-key" searchReq
    assertAck gatewayResponse

    searchEndResult <- waitForCallback (onSearchEndMVar callbackData)

    case searchEndResult of
      Just res -> do
        verifyApiKey $ apiKey res
        verifyCallbackContext False transactionId $ result res ^. #context
      Nothing -> expectationFailure "No search end callback received."

    searchResults <- processResults transactionId callbackData

    let dunzoResults = filter isDunzoResult searchResults

    case rights (map contents dunzoResults) of
      [message] ->
        verifyDunzoCatalog message
      _ -> expectationFailure "Expected one search result from Dunzo."
  where
    isDunzoResult result =
      _bpp_uri (context result) == Just fmdWrapperBaseUrl

dunzoUnserviceableLocation :: ClientEnv -> CallbackData -> IO ()
dunzoUnserviceableLocation =
  dunzoLocationError gps1 example $
    Error.Error "DOMAIN-ERROR" "FMD001" Nothing $
      Just "Location is not serviceable."

dunzoNearByLocation :: ClientEnv -> CallbackData -> IO ()
dunzoNearByLocation =
  dunzoLocationError gps1 gps1 $
    Error.Error "DOMAIN-ERROR" "FMD001" Nothing $
      Just "The pickup and drop addresses are near by."

dunzoDifferentCity :: ClientEnv -> CallbackData -> IO ()
dunzoDifferentCity =
  dunzoLocationError gps1 gps3 $
    Error.Error "DOMAIN-ERROR" "FMD001" Nothing $
      Just "Apologies, our services are limited to serviceable areas with in the city only."

incorrectApiKey :: ClientEnv -> CallbackData -> IO ()
incorrectApiKey clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {_core_version = Nothing}

  gatewayResponse <- runSearch clientEnv "some-key" searchReq
  verifyError 401 "INVALID_API_KEY" gatewayResponse

incorrectAction :: ClientEnv -> CallbackData -> IO ()
incorrectAction clientEnv _ = do
  ctx <- buildContext "" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx

  gatewayResponse <- runSearch clientEnv "fmd-test-app-key" searchReq
  verifyError 400 "INVALID_ACTION" gatewayResponse

incorrectCountry :: ClientEnv -> CallbackData -> IO ()
incorrectCountry clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {_country = Just ""}

  gatewayResponse <- runSearch clientEnv "fmd-test-app-key" searchReq
  verifyError 400 "INVALID_COUNTRY" gatewayResponse

incorrectDomainVersion :: ClientEnv -> CallbackData -> IO ()
incorrectDomainVersion clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {_domain_version = Just "0.7.0"}

  gatewayResponse <- runSearch clientEnv "fmd-test-app-key" searchReq
  verifyError 400 "UNSUPPORTED_DOMAIN_VERSION" gatewayResponse

incorrectCoreVersion :: ClientEnv -> CallbackData -> IO ()
incorrectCoreVersion clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {_core_version = Just "0.7.0"}

  gatewayResponse <- runSearch clientEnv "fmd-test-app-key" searchReq
  verifyError 400 "UNSUPPORTED_CORE_VERSION" gatewayResponse

missingBapUri :: ClientEnv -> CallbackData -> IO ()
missingBapUri clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" Nothing Nothing
  let searchReq = buildFMDSearchReq ctx

  gatewayResponse <- runSearch clientEnv "fmd-test-app-key" searchReq
  verifyError 400 "INVALID_BAP_URI" gatewayResponse

missingCountry :: ClientEnv -> CallbackData -> IO ()
missingCountry clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {_country = Nothing}

  gatewayResponse <- runSearch clientEnv "fmd-test-app-key" searchReq
  verifyError 400 "INVALID_COUNTRY" gatewayResponse

missingDomainVersion :: ClientEnv -> CallbackData -> IO ()
missingDomainVersion clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {_domain_version = Nothing}

  gatewayResponse <- runSearch clientEnv "fmd-test-app-key" searchReq
  verifyError 400 "UNSUPPORTED_DOMAIN_VERSION" gatewayResponse

missingCoreVersion :: ClientEnv -> CallbackData -> IO ()
missingCoreVersion clientEnv _ = do
  ctx <- buildContext "search" "dummy-txn-id" (Just fmdTestAppBaseUrl) Nothing
  let searchReq = buildFMDSearchReq ctx {_core_version = Nothing}

  gatewayResponse <- runSearch clientEnv "fmd-test-app-key" searchReq
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
