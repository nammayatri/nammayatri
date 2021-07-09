{-# LANGUAGE OverloadedLabels #-}

module FmdWrapper.Flow.Confirm where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.Address (Address (..))
import Beckn.Types.Core.Migration.Billing ()
import Beckn.Utils.Example (example)
import Common (signRequest, verifyError)
import Control.Lens.Prism (_Just)
import qualified Data.HashMap.Strict as HMS
import Data.Time.Clock.POSIX (getPOSIXTime)
import EulerHS.Prelude
import Fmd (buildContext, fmdWrapperBaseUrl)
import FmdWrapper.Common (assertAck, withNewUUID)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant (Header, (:>))
import Servant.Client (ClientEnv, ClientError, client, mkClientEnv)
import Test.Hspec hiding (example)
import qualified "fmd-wrapper" Types.Beckn.API.Confirm as ConfirmAPI
import qualified "fmd-wrapper" Types.Beckn.API.Types as API
import "fmd-wrapper" Types.Beckn.Contact (Contact (..))
import "fmd-wrapper" Types.Beckn.Context (Action (..))
import "fmd-wrapper" Types.Beckn.Fulfillment (Fulfillment (..), FulfillmentDetails (..))
import "fmd-wrapper" Types.Beckn.Gps (Gps (..))
import "fmd-wrapper" Types.Beckn.ItemQuantity (emptyItemQuantity)
import "fmd-wrapper" Types.Beckn.Location (Location (..))
import "fmd-wrapper" Types.Beckn.Order (IdAndLocations (..), Order (..), OrderItem (..))
import "fmd-wrapper" Types.Beckn.Payment (Params (..), Payment (..))
import "fmd-wrapper" Types.Beckn.Person (Person (..))
import "fmd-wrapper" Types.Beckn.Quotation (Quotation (..))
import Utils (runClient)

confirmOrder :: API.OrderObject
confirmOrder =
  API.OrderObject $
    Order
      { id = Nothing,
        state = Nothing,
        provider =
          IdAndLocations
            { id = "",
              locations = []
            },
        items = [OrderItem "2" emptyItemQuantity],
        add_ons = [],
        offers = [],
        billing = example,
        fulfillment =
          Fulfillment
            { id = Nothing,
              _type = Nothing,
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
                              gps = Just $ Gps 12.9729391 77.6294794,
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
                              gps = Just $ Gps 12.9354504 77.6146828,
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
              purpose = Nothing,
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
                      amount = Nothing,
                      additional = HMS.empty
                    },
              payee = Nothing,
              _type = Nothing,
              status = Nothing,
              time = Nothing
            },
        created_at = Nothing,
        updated_at = Nothing
      }

runConfirm :: ClientEnv -> Text -> API.BecknReq API.OrderObject -> IO (Either ClientError AckResponse)
runConfirm clientEnv orgId confirmReq = do
  now <- getPOSIXTime
  let signature = decodeUtf8 $ signRequest confirmReq now orgId (orgId <> "-key")
  let confirmAPI = Proxy :: Proxy (Header "Authorization" Text :> ConfirmAPI.ConfirmAPI)
  runClient clientEnv $ client confirmAPI (Just signature) confirmReq

successfulConfirm :: ClientEnv -> IO ()
successfulConfirm clientEnv =
  withNewUUID $ \transactionId -> do
    ctx <- buildContext CONFIRM transactionId
    let confirmReq = API.BecknReq ctx confirmOrder
    response <- runConfirm clientEnv "fmd-test-app" confirmReq
    assertAck response

confirmWithoutPaymentTransactionId :: ClientEnv -> IO ()
confirmWithoutPaymentTransactionId clientEnv =
  withNewUUID $ \transactionId -> do
    ctx <- buildContext CONFIRM transactionId
    let orderWithoutTrnsxnId = confirmOrder & #order . #payment . #params . _Just . #transaction_id .~ Nothing
    let confirmReq = API.BecknReq ctx orderWithoutTrnsxnId
    response <- runConfirm clientEnv "fmd-test-app" confirmReq
    verifyError 400 "TXN_ID_NOT_PRESENT" response

spec :: Spec
spec = do
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let appClientEnv = mkClientEnv appManager fmdWrapperBaseUrl
  describe "Confirm API" do
    it "Successful confirm" $ successfulConfirm appClientEnv
    it "Fail if payment transaction id not found" $ confirmWithoutPaymentTransactionId appClientEnv
