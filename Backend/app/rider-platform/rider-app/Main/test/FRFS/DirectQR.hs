module FRFS.DirectQR where

import App.Server
import Control.Exception.Safe (SomeException)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day (..), UTCTime (..), secondsToDiffTime)
import Data.Time.Clock (NominalDiffTime)
import Domain.Types.IntegratedBPPConfig
import Environment
import qualified EulerHS.Interpreters as R
import EulerHS.Prelude
import qualified EulerHS.Runtime as ER
import ExternalBPP.ExternalAPI.Direct.Utils
import ExternalBPP.ExternalAPI.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Base64
import Kernel.Types.Flow
import Kernel.Utils.Common
import Kernel.Utils.FlowLogging
import qualified Kernel.Utils.SignatureAuth as HttpSig
import Test.Tasty
import Test.Tasty.HUnit
import Tools.Error

-- Test configuration as JSON string
testConfigJson :: Text
testConfigJson = T.pack "{\"cipherKey\":\"4nLE9cN2golcxW16Df5aOcajSJqDAupo2B4rLcczKEI=\",\"qrRefreshTtl\":3600}"

-- Test ticket payload as JSON string
testTicketJson :: Text
testTicketJson = T.pack "{\"ticketNumber\":\"313356\",\"fromRouteProviderCode\":\"SCM\",\"toRouteProviderCode\":\"SKO\",\"adultQuantity\":2,\"childQuantity\":0,\"vehicleTypeProviderCode\":\"1\",\"ticketAmount\":5,\"expiry\":\"2025-10-01T00:00:00Z\",\"refreshAt\":\"2025-09-01T00:00:00Z\"}"

decodeFromTextEither :: (FromJSON a) => Text -> Either String a
decodeFromTextEither = A.eitherDecode . BSL.fromStrict . TE.encodeUtf8

tests :: ER.FlowRuntime -> AppEnv -> Flow ()
tests flowRt' appEnv = do
  logInfo "Running QR code generation and decoding functions..."

  -- Generate key pair
  logInfo "Generating key pair..."
  (privateKey, publicKey) <- liftIO $ HttpSig.generateKeyPair
  logInfo $ "Private key: " <> (TE.decodeUtf8 $ BSL.toStrict $ A.encode privateKey)
  logInfo $ "Public key: " <> (TE.decodeUtf8 $ BSL.toStrict $ A.encode publicKey)

  -- Decode test configuration
  logInfo "Decoding test configuration..."
  testConfig <-
    decodeFromTextEither @DIRECTConfig testConfigJson & \case
      Right config -> pure config
      Left err -> throwError $ InvalidRequest $ "Failed to decode test config: " <> show err

  -- Decode test ticket
  logInfo "Decoding test ticket..."
  testTicket <-
    decodeFromTextEither @TicketPayload testTicketJson & \case
      Right ticket -> pure ticket
      Left err -> throwError $ InvalidRequest $ "Failed to decode test ticket: " <> show err

  -- Generate QR code
  logInfo "Generating QR code..."
  qrData <- generateQR testConfig testTicket
  logInfo $ "Generated QR data: " <> qrData

  -- Decode QR code
  logInfo "Decoding QR code..."
  decodedTicket <- decodeQR testConfig qrData
  logInfo $ "Decoded ticket: " <> show decodedTicket

  -- Try QR code refresh
  logInfo "Attempting QR code refresh..."
  mbRefreshed <- refreshQR testConfig qrData
  case mbRefreshed of
    Just (newQrData, newRefreshAt) -> do
      logInfo $ "New QR data: " <> newQrData
      logInfo $ "New refresh time: " <> show newRefreshAt
    Nothing -> logInfo "QR code refresh failed"

  -- Try invalid QR code
  logInfo "Testing invalid QR code..."
  result <- withTryCatch "decodeQR:testInvalidQR" $ decodeQR testConfig "invalid_qr_data"
  case result of
    Left err -> logInfo $ "Expected error for invalid QR: " <> show err
    Right _ -> logInfo "Unexpected success for invalid QR"
