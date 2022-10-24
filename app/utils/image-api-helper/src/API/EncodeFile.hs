module API.EncodeFile where

import Beckn.Mock.App hiding (runMock)
import Beckn.Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.String.Conversions
import Environment
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
