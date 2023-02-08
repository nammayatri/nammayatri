module API.DecodeFile where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.String.Conversions
import Environment
import Kernel.Mock.App hiding (runMock)
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Utils.Common
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
  let path = cs req.filePath
  raw <- B64.decode (cs req.base64) & fromEitherM (InvalidRequest . cs)
  liftIO $ B.writeFile path raw
  pure "OK"
