{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Utils where

import Data.Aeson (decode)
import Data.String.Conversions
import qualified "rider-app" Domain.Types.Booking as BDB
import qualified "rider-app" Domain.Types.Ride as BDRide
import qualified "dynamic-offer-driver-app" Environment as ARDU
import qualified "rider-app" Environment as BecknApp
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import GHC.IO (unsafePerformIO)
import HSpec
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Types.Id (Id (Id))
import Kernel.Utils.Common
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Resources (appBackendEnv, driverOfferBppEnv)
import Servant.Client hiding (client)
import qualified "rider-app" Storage.Queries.Booking as BQB
import qualified "rider-app" Storage.Queries.RegistrationToken as BQRegToken
import qualified "rider-app" Storage.Queries.Ride as BQRide

defaultTestLoggerConfig :: LoggerConfig
defaultTestLoggerConfig =
  LoggerConfig
    { level = DEBUG,
      logToFile = True,
      logFilePath = "/tmp/beckn-integ-test.log",
      logToConsole = False,
      logRawSql = True,
      prettyPrinting = True
    }

runClient :: (HasCallStack, MonadIO m) => ClientEnv -> ClientM a -> m (Either ClientError a)
runClient clientEnv x = liftIO $ runClientM x clientEnv

runClient' :: (HasCallStack, MonadIO m, Show a) => ClientEnv -> ClientM a -> m a
runClient' clientEnv x = do
  res <- runClient clientEnv x
  res `shouldSatisfy` isRight
  let Right r = res
  return r

-- | Invoke an action until getting 'Just'.
--
-- The second argument describes attempted delays prior to running an action,
-- in mcs.
pollWithDescription :: (HasCallStack, MonadIO m, MonadCatch m) => Text -> [Int] -> m (Maybe a) -> m a
pollWithDescription description allDelays action = withFrozenCallStack $ go allDelays
  where
    go [] =
      expectationFailure $
        "poll: failed to get an expected entry after "
          <> show (fromIntegral (sum allDelays) / 1e6 :: Float)
          <> " seconds;\ndescription: "
          <> cs description
    go (delay : remDelays) = do
      let printLastError err = do
            when (null remDelays) $ print ("Last error: " <> show err :: Text)
            return Nothing
      liftIO $ threadDelay delay
      try @_ @SomeException action >>= either printLastError return >>= maybe (go remDelays) pure

expBackoff :: Int -> Int -> [Int]
expBackoff startDelay maxDelay =
  0 : takeWhile (< maxDelay) (iterate (* 2) startDelay)

-- | 'pollWith' with default timing.
--
-- Optimized for requesting a server for a result of async action.
poll :: (HasCallStack, MonadIO m, MonadCatch m) => m (Maybe a) -> m a
poll = pollDesc ""

pollDesc :: (HasCallStack, MonadIO m, MonadCatch m) => Text -> m (Maybe a) -> m a
pollDesc description = pollWithDescription description (expBackoff 0.1e6 10e6)

pollList :: (HasCallStack, MonadIO m, MonadCatch m) => Text -> m [a] -> m (NonEmpty a)
pollList description action = pollDesc description $ nonEmpty <$> action

pollFilteredList :: (HasCallStack, MonadIO m, MonadCatch m) => Text -> (a -> Bool) -> m [a] -> m (NonEmpty a)
pollFilteredList description filterFunc action =
  pollDesc description $ nonEmpty . filter filterFunc <$> action

pollFilteredMList :: (HasCallStack, MonadIO m, MonadCatch m) => Text -> (a -> m Bool) -> m [a] -> m (NonEmpty a)
pollFilteredMList description filterFunc action =
  pollDesc description $ nonEmpty <$> (action >>= filterM filterFunc)

runFlow :: (MonadIO m, Log (FlowR env)) => Text -> env -> FlowR env a -> m a
runFlow tag appEnv flow = do
  liftIO $
    R.withFlowRuntime Nothing $ \flowRt -> do
      runFlowR flowRt appEnv $ withLogTag tag flow

expectSingletonNE :: (HasCallStack, MonadIO m) => NonEmpty a -> m a
expectSingletonNE = \case
  a :| [] -> pure a
  l -> expectationFailure $ "Expected list with one element, got " <> show (length l) <> "elements"

expectSingletonList :: (HasCallStack, MonadIO m) => [a] -> m a
expectSingletonList = \case
  [a] -> pure a
  l -> expectationFailure $ "Expected list with one element, got " <> show (length l) <> "elements"

data ClientEnvs = ClientEnvs
  { bap :: ClientEnv,
    bpp :: ClientEnv
  }

type ClientsM = ReaderT ClientEnvs IO

withBecknClients :: ClientEnvs -> ClientsM a -> IO a
withBecknClients = flip runReaderT

callBAP, callBPP :: (HasCallStack, Show a) => ClientM a -> ClientsM a
callBAP client = asks (.bap) >>= (`runClient'` client)
callBPP client = asks (.bpp) >>= (`runClient'` client)

callBppEither :: ClientM a -> ClientsM (Either ClientError a)
callBppEither client = asks (.bpp) >>= (`runClient` client)

mkMobilityClients :: BaseUrl -> BaseUrl -> IO ClientEnvs
mkMobilityClients bapUrl bppUrl = do
  pure $
    ClientEnvs
      { bap = mkClientEnv defaultManager bapUrl,
        bpp = mkClientEnv defaultManager bppUrl
      }

{-# NOINLINE defaultManager #-}
defaultManager :: Manager
defaultManager = unsafePerformIO $ Client.newManager tlsManagerSettings

runAppFlow :: Text -> FlowR BecknApp.AppEnv a -> IO a
runAppFlow tag = runFlow tag appBackendEnv

runARDUFlow :: Text -> FlowR ARDU.AppEnv a -> IO a
runARDUFlow tag = runFlow tag driverOfferBppEnv

data Person

data DriverTestData = DriverTestData
  { driverId :: Id Person,
    token :: Text
  }

shouldReturnErrorCode :: MonadIO m => Text -> Text -> Either ClientError a -> m ()
shouldReturnErrorCode description code eithRes =
  case eithRes of
    Left (FailureResponse _ offerRes) -> do
      let offerResBody = responseBody offerRes
          mbAPIError = decode offerResBody :: Maybe APIError
      fmap (.errorCode) mbAPIError `shouldBe` Just code
    Left _ -> expectationFailure $ cs $ description <> ": unexpected error"
    Right _ -> expectationFailure $ cs $ description <> ": unexpected success"

equalsEps :: (Ord a, Num a) => a -> a -> a -> Bool
equalsEps eps x y = abs (x - y) < eps

equals :: (Container l, Eq (Element l)) => l -> l -> Bool
equals list1 list2 = all (`elem` list1) list2 && all (`elem` list2) list1

resetCustomer :: Text -> IO ()
resetCustomer token = runAppFlow "" $ do
  regToken <- BQRegToken.findByToken token >>= fromMaybeM (InvalidToken token)
  activeBookings <- BQB.findByRiderIdAndStatus (Id regToken.entityId) BDB.activeBookingStatusForCheck
  forM_ activeBookings $ \activeBooking -> do
    rides <- BQRide.findActiveByRBId activeBooking.id
    _ <- BQB.updateStatus activeBooking.id BDB.CANCELLED
    void . forM rides $ \ride ->
      BQRide.updateStatus ride.id BDRide.CANCELLED

beforeAndAfter_ :: IO () -> SpecWith a -> SpecWith a
beforeAndAfter_ f = after_ f . before_ f
