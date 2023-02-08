module API.Total where

import API.DecodeFile
import API.EncodeFile
import Environment
import Kernel.Mock.App
import Kernel.Prelude
import Servant

type TotalAPI = EncodeFileAPI :<|> DecodeFileAPI

totalAPI :: Proxy TotalAPI
totalAPI = Proxy

totalServer :: ServerT TotalAPI (MockM AppEnv)
totalServer = encodeFileHandler :<|> decodeFileHandler
