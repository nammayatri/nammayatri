module FmdWrapper.Flow.Init where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.Billing ()
import Beckn.Utils.Example (example)
import Common (signRequest)
import Data.Time.Clock.POSIX (getPOSIXTime)
import EulerHS.Prelude
import Fmd (buildContext, fmdWrapperBaseUrl)
import FmdWrapper.Common (assertAck, withNewUUID)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant (Header, (:>))
import Servant.Client (ClientEnv, ClientError, client, mkClientEnv)
import Test.Hspec hiding (example)
import qualified "fmd-wrapper" Types.Beckn.API.Init as InitAPI
import qualified "fmd-wrapper" Types.Beckn.API.Types as API
import "fmd-wrapper" Types.Beckn.Context (Action (..))
import "fmd-wrapper" Types.Beckn.Fulfillment (Fulfillment (..), FulfillmentDetails (..))
import "fmd-wrapper" Types.Beckn.Gps (Gps (..))
import "fmd-wrapper" Types.Beckn.ItemQuantity (Quantity (..))
import "fmd-wrapper" Types.Beckn.Location (Location (..))
import Utils (runClient)

initOrder :: InitAPI.InitOrderObj
initOrder =
  InitAPI.InitOrderObj $
    InitAPI.InitOrder
      { provider =
          InitAPI.InitOrderProvider
            { id = "",
              locations = []
            },
        items = [InitAPI.InitOrderItem "" (Quantity (Just 1) Nothing)],
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
                              address = Nothing,
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
                      contact = Nothing,
                      person = Nothing
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
                              address = Nothing,
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
                      contact = Nothing,
                      person = Nothing
                    },
              purpose = Nothing,
              tags = Nothing
            }
      }

runInit :: ClientEnv -> Text -> API.BecknReq InitAPI.InitOrderObj -> IO (Either ClientError AckResponse)
runInit clientEnv orgId initReq = do
  now <- getPOSIXTime
  let signature = decodeUtf8 $ signRequest initReq now orgId (orgId <> "-key")
  let initAPI = Proxy :: Proxy (Header "Authorization" Text :> InitAPI.InitAPI)
  runClient clientEnv $ client initAPI (Just signature) initReq

successfulInit :: ClientEnv -> IO ()
successfulInit clientEnv =
  withNewUUID $ \transactionId -> do
    ctx <- buildContext INIT transactionId
    let initReq = API.BecknReq ctx initOrder
    response <- runInit clientEnv "fmd-test-app" initReq
    assertAck response

spec :: Spec
spec = do
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let appClientEnv = mkClientEnv appManager fmdWrapperBaseUrl
  describe "Init API" $
    it "Successful init" $ successfulInit appClientEnv
