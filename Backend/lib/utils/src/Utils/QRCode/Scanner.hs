{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Utils.QRCode.Scanner
  ( scanQRCode,
  )
where

import Control.Exception (try)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.Prelude hiding (handle, putStrLn, try)
import System.Exit
import System.IO (hClose, hFlush)
import System.IO.Temp (withSystemTempFile)
import System.Process (readCreateProcessWithExitCode, shell)
import qualified Prelude (putStrLn)

-- | Scan QR code from image bytes
-- Returns the decoded text content of the QR code if found
scanQRCode :: BL.ByteString -> IO (Maybe Text)
scanQRCode imgBytes = do
  -- We use the zbar tool which is a common QR code reader
  -- This requires the zbarimg command to be installed on the system
  result <- withSystemTempFile "qrcode.png" $ \tmpFile handle -> do
    -- Write image bytes to temp file
    BL.hPut handle imgBytes
    hFlush handle
    hClose handle

    -- Call zbarimg to scan QR code
    try @SomeException $ readCreateProcessWithExitCode (shell $ "zbarimg " <> tmpFile) ""

  case result of
    Left err -> do
      -- Log error but don't fail
      Prelude.putStrLn $ "Error scanning QR code: " <> show err
      pure Nothing
    Right (ExitSuccess, output, _) ->
      -- If output is empty, no QR code was found
      if null output
        then pure Nothing
        else pure $ Just $ T.strip $ T.replace "QR-Code:" "" $ TE.decodeUtf8 $ BL.toStrict $ fromString output
    Right (_, _, err) -> do
      Prelude.putStrLn $ "Error code returned from command: " <> show err
      pure Nothing
