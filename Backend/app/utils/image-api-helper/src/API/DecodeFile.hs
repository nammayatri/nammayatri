{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.DecodeFile where

import Data.Char (isAlphaNum)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.String.Conversions
import qualified Data.Text as T
import Environment
import Kernel.Mock.App hiding (runMock)
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Utils.Common (fromEitherM, fromMaybeM)
import Servant

type DecodeFileAPI =
  "decode"
    :> ReqBody '[JSON] DecodeFileReq
    :> Post '[JSON] Text

data DecodeFileReq = DecodeFileReq
  { filePath :: Text,
    base64 :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

decodeFileHandler :: DecodeFileReq -> MockM AppEnv Text
decodeFileHandler req = {- apiHandler $ -} do
  path <- toSandboxedPath req.filePath & fromMaybeM (InvalidRequest "filePath must be a simple file name")
  raw <- B64.decode (cs req.base64) & fromEitherM (InvalidRequest . cs)
  liftIO $ B.writeFile path raw
  pure "OK"

toSandboxedPath :: Text -> Maybe FilePath
toSandboxedPath rawName = do
  let fileName = T.strip rawName
  guard $ isValidFileName fileName
  pure $ "/tmp/image-api-helper-" <> T.unpack fileName

isValidFileName :: Text -> Bool
isValidFileName fileName =
  not (T.null fileName)
    && T.length fileName <= 128
    && fileName /= "."
    && fileName /= ".."
    && T.all isSafeFileNameChar fileName

isSafeFileNameChar :: Char -> Bool
isSafeFileNameChar char = isAlphaNum char || char `elem` ['.', '_', '-']
