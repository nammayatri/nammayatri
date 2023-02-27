{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.EncodeFile where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.String.Conversions
import Environment
import Kernel.Mock.App hiding (runMock)
import Kernel.Prelude
import Servant

type EncodeFileAPI =
  "encode"
    :> ReqBody '[JSON] EncodeFileReq
    :> Get '[JSON] EncodeFileResp

newtype EncodeFileReq = EncodeFileReq
  { filePath :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

newtype EncodeFileResp = EncodeFileResp
  { base64 :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

encodeFileHandler :: EncodeFileReq -> MockM AppEnv EncodeFileResp
encodeFileHandler req = do
  let path = req.filePath
  raw <- liftIO $ B.readFile $ cs path
  let base64 = cs $ B64.encode raw
  pure EncodeFileResp {base64}
