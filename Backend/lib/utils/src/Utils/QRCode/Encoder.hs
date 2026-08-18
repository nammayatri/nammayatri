{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Utils.QRCode.Encoder
  ( encodeQRCodePngDataUri,
  )
where

import Control.Exception (try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.Prelude hiding (handle, try)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (proc, readCreateProcessWithExitCode)

-- | Encode arbitrary text into a QR-code PNG via the @qrencode@ CLI and return
-- a base64 @data:@ URI ready for an @\<img src\>@. Returns 'Nothing' on failure
-- (missing binary, empty content, non-zero exit) — callers treat the QR as
-- optional and simply omit the image.
--
-- The payload is fed on stdin, so newlines / @₹@ / colons in the content need
-- no shell escaping. Requires the @qrencode@ binary on PATH (provided via nix,
-- mirroring the @zbar@ dependency of "Utils.QRCode.Scanner").
encodeQRCodePngDataUri :: Text -> IO (Maybe Text)
encodeQRCodePngDataUri content
  | T.null (T.strip content) = pure Nothing
  | otherwise =
    withSystemTempFile "qrcode.png" $ \tmpFile handle -> do
      hClose handle
      result <-
        try @SomeException $
          readCreateProcessWithExitCode
            (proc "qrencode" ["-8", "-s", "6", "-m", "2", "-t", "PNG", "-o", tmpFile])
            (T.unpack content)
      case result of
        Right (ExitSuccess, _, _) -> do
          bytes <- BS.readFile tmpFile
          pure $
            if BS.null bytes
              then Nothing
              else Just ("data:image/png;base64," <> TE.decodeUtf8 (B64.encode bytes))
        _ -> pure Nothing
