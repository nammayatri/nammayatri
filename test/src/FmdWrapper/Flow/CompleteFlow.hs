{-# LANGUAGE OverloadedLabels #-}

module FmdWrapper.Flow.CompleteFlow where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.Address (Address (..))
import Beckn.Types.Core.Migration.Billing ()
import Beckn.Types.Core.Migration.Intent (Intent)
import Beckn.Utils.Example (example)
import Common (gatewayBaseUrl, signRequest)
import Control.Concurrent.MVar (isEmptyMVar)
import Control.Lens ((?~), _Just)
import qualified Data.HashMap.Strict as HMS
import Data.Time.Clock.POSIX (getPOSIXTime)
import EulerHS.Prelude
import Fmd (buildContext, buildFMDSearchReq, fmdWrapperBaseUrl)
import FmdWrapper.Common (assertAck, setIntentDropGps, setIntentPickupGps, withNewUUID)
import qualified FmdWrapper.Fixtures as Fixtures
import FmdWrapper.Flow.Confirm (runConfirm)
import FmdWrapper.Flow.Search (processResults, runSearch, verifyDunzoCatalog)
import FmdWrapper.Server (CallbackData, waitForCallback, withCallbackApp)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant (Header, (:>))
import Servant.Client (Client, ClientEnv (..), ClientError, ClientM, HasClient, client, mkClientEnv)
import Test.Hspec hiding (example)
import qualified "fmd-wrapper" Types.Beckn.API.Cancel as CancelAPI
import qualified "fmd-wrapper" Types.Beckn.API.Status as StatusAPI
import qualified "fmd-wrapper" Types.Beckn.API.Track as TrackAPI
import qualified "fmd-wrapper" Types.Beckn.API.Types as API
import "fmd-wrapper" Types.Beckn.Catalog (Catalog)
import "fmd-wrapper" Types.Beckn.Contact (Contact (..))
import "fmd-wrapper" Types.Beckn.Context (Action (..), Context (..))
import "fmd-wrapper" Types.Beckn.Fulfillment (Fulfillment (..), FulfillmentDetails (..))
import "fmd-wrapper" Types.Beckn.ItemQuantity (emptyItemQuantity)
import "fmd-wrapper" Types.Beckn.Location (Location (..))
import "fmd-wrapper" Types.Beckn.Order (Order (..), OrderItem (..))
import "fmd-wrapper" Types.Beckn.Payment (Params (..), Payment (..))
import "fmd-wrapper" Types.Beckn.Person (Person (..))
import "fmd-wrapper" Types.Beckn.Quotation (Quotation (..))
import Utils (runClient)

mkConfirmOrderFromSearchData :: Intent -> Catalog -> API.OrderObject
mkConfirmOrderFromSearchData intent catalog =
  API.OrderObject $
    Order
      { id = Nothing,
        state = Nothing,
        items = [OrderItem clothesAndAccessoriesItemId emptyItemQuantity],
        add_ons = [],
        offers = [],
        billing = example,
        fulfillment =
          Fulfillment
            { id = Nothing,
              _type = Nothing,
              provider_id = Nothing,
              state = Nothing,
              tracking = False,
              customer = Nothing,
              agent = Nothing,
              vehicle = Nothing,
              start =
                Just $
                  FulfillmentDetails
                    { location =
                        Just
                          Location
                            { id = Nothing,
                              descriptor = Nothing,
                              gps = intent.fulfillment ^? _Just . #start . _Just . #location . _Just . #gps . _Just,
                              address =
                                Just $
                                  Address
                                    { door = Just "#444",
                                      name = Nothing,
                                      building = Nothing,
                                      street = Just "18th Main",
                                      locality = Nothing,
                                      ward = Nothing,
                                      city = Just "Bangalore",
                                      state = Just "Karnataka",
                                      country = Just "India",
                                      area_code = Just "560047"
                                    },
                              station_code = Nothing,
                              city = Nothing,
                              country = Nothing,
                              circle = Nothing,
                              polygon = Nothing,
                              _3dspace = Nothing,
                              time = Nothing
                            },
                      time = Nothing,
                      instructions = Nothing,
                      contact = Just $ Contact (Just "+919999999999") Nothing Nothing,
                      person = Just $ Person Nothing Nothing Nothing Nothing Nothing Nothing
                    },
              end =
                Just
                  FulfillmentDetails
                    { location =
                        Just
                          Location
                            { id = Nothing,
                              descriptor = Nothing,
                              gps = intent.fulfillment ^? _Just . #end . _Just . #location . _Just . #gps . _Just,
                              address =
                                Just $
                                  Address
                                    { door = Just "#444",
                                      name = Nothing,
                                      building = Nothing,
                                      street = Just "18th Main",
                                      locality = Nothing,
                                      ward = Nothing,
                                      city = Just "Bangalore",
                                      state = Just "Karnataka",
                                      country = Just "India",
                                      area_code = Just "560047"
                                    },
                              station_code = Nothing,
                              city = Nothing,
                              country = Nothing,
                              circle = Nothing,
                              polygon = Nothing,
                              _3dspace = Nothing,
                              time = Nothing
                            },
                      time = Nothing,
                      instructions = Nothing,
                      contact = Just $ Contact (Just "+919999999999") Nothing Nothing,
                      person = Just $ Person Nothing Nothing Nothing Nothing Nothing Nothing
                    },
              tags = Nothing
            },
        quote =
          Quotation
            { price = Nothing,
              breakup = Nothing,
              ttl = Nothing
            },
        payment =
          Payment
            { uri = Nothing,
              tl_method = Nothing,
              params =
                Just $
                  Params
                    { transaction_id = Just "transaction id",
                      transaction_status = Nothing,
                      amount = Nothing,
                      currency = "INR",
                      additional = HMS.empty
                    },
              _type = Nothing,
              status = Nothing,
              time = Nothing
            },
        created_at = Nothing,
        updated_at = Nothing
      }
  where
    clothesAndAccessoriesItemId =
      let (provider : _xs) = fromJust catalog.bpp_providers
          (_item1 : item2 : _items) = fromJust provider.items
       in fromJust item2.id

runClientCall ::
  (ToJSON req, HasClient ClientM api, Client ClientM api ~ (Maybe Text -> req -> ClientM resp)) =>
  ClientEnv ->
  Text ->
  Proxy api ->
  req ->
  IO (Either ClientError resp)
runClientCall clientEnv orgId api req = do
  now <- getPOSIXTime
  let signature :: Text = decodeUtf8 $ signRequest req now orgId (orgId <> "-key")
  runClient clientEnv $ client api (Just signature) req

runStatus :: ClientEnv -> Text -> API.BecknReq StatusAPI.OrderId -> IO (Either ClientError AckResponse)
runStatus clientEnv orgId statusReq = do
  let statusAPI = Proxy :: Proxy (Header "Authorization" Text :> StatusAPI.StatusAPI)
  runClientCall clientEnv orgId statusAPI statusReq

runTrack :: ClientEnv -> Text -> API.BecknReq TrackAPI.TrackInfo -> IO (Either ClientError AckResponse)
runTrack clientEnv orgId trackReq = do
  let trackAPI = Proxy :: Proxy (Header "Authorization" Text :> TrackAPI.TrackAPI)
  runClientCall clientEnv orgId trackAPI trackReq

runCancel :: ClientEnv -> Text -> API.BecknReq CancelAPI.CancellationInfo -> IO (Either ClientError AckResponse)
runCancel clientEnv orgId cancelReq = do
  let cancelAPI = Proxy :: Proxy (Header "Authorization" Text :> CancelAPI.CancelAPI)
  runClientCall clientEnv orgId cancelAPI cancelReq

successfulFlow :: ClientEnv -> CallbackData -> IO ()
successfulFlow clientEnv callbackData = withNewUUID $ \transactionId -> do
  ctx <- buildContext SEARCH transactionId
  let searchReq =
        buildFMDSearchReq ctx
          & setIntentPickupGps ?~ Fixtures.validDunzoGps1
          & setIntentDropGps ?~ Fixtures.validDunzoGps2
  gatewayResponse <- runSearch clientEnv "fmd-test-app" searchReq
  assertAck gatewayResponse

  waitForCallback
  searchResults <- processResults transactionId callbackData
  let dunzoResults = filter isDunzoResult searchResults

  let searchRes = rights (map API.contents dunzoResults)
  case searchRes of
    [] -> expectationFailure "Expected search result from Dunzo."
    (latestSearchCatalog : _xs) -> do
      verifyDunzoCatalog latestSearchCatalog

      let confirmCtx = ctx {action = CONFIRM}
      let confirmReq =
            API.BecknReq confirmCtx $
              mkConfirmOrderFromSearchData searchReq.message.intent latestSearchCatalog.catalog
      let fmdClientEnv = clientEnv {baseUrl = fmdWrapperBaseUrl}
      confirmResponse <- runConfirm fmdClientEnv "fmd-test-app" confirmReq
      assertAck confirmResponse

      waitForCallback
      mbConfirmCb <- tryGetCallback callbackData.onConfirmCb
      case mbConfirmCb of
        Nothing -> expectationFailure "Expected successful confirm callback from BPP."
        Just confirmCb -> do
          let mbOrderId = confirmCb.order.id
          case mbOrderId of
            Nothing -> expectationFailure "Expected order_id in confirm callback from BPP."
            Just orderId -> do
              let trackReq = API.BecknReq ctx {action = TRACK} (TrackAPI.TrackInfo orderId Nothing)
              trackResponse <- runTrack fmdClientEnv "fmd-test-app" trackReq
              checkResponseAndCallback trackResponse "track" callbackData.onTrackCb

              let statusReq = API.BecknReq ctx {action = STATUS} (StatusAPI.OrderId orderId)
              statusResponse <- runStatus fmdClientEnv "fmd-test-app" statusReq
              checkResponseAndCallback statusResponse "status" callbackData.onStatusCb

              let cancelReq = API.BecknReq ctx {action = CANCEL} (CancelAPI.CancellationInfo orderId "Not specified." Nothing)
              cancelResponse <- runCancel fmdClientEnv "fmd-test-app" cancelReq
              checkResponseAndCallback cancelResponse "cancel" callbackData.onCancelCb
  where
    isDunzoResult result =
      result.context.bpp_uri == Just fmdWrapperBaseUrl
    tryGetCallback cbMVar = do
      callbackNotReceived <- isEmptyMVar cbMVar
      if callbackNotReceived
        then pure Nothing
        else readMVar cbMVar <&> rightToMaybe . (.result.contents)
    checkResponseAndCallback response api callbackMVar = do
      assertAck response
      waitForCallback
      mbCallback <- tryGetCallback callbackMVar
      whenNothing_ mbCallback $
        expectationFailure ("Expected successful " <> api <> " callback from BPP.")

spec :: Spec
spec = do
  around withCallbackApp $ do
    appManager <- runIO $ Client.newManager tlsManagerSettings
    let appClientEnv = mkClientEnv appManager gatewayBaseUrl
    it "Successful flow pass" $ successfulFlow appClientEnv
