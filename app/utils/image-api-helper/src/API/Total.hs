module API.Total where

import API.DecodeFile
import API.EncodeFile
import Beckn.Mock.App
import Beckn.Prelude
import Environment
import Servant

type TotalAPI = EncodeFileAPI :<|> DecodeFileAPI

totalAPI :: Proxy TotalAPI
totalAPI = Proxy

totalServer :: ServerT TotalAPI (MockM AppEnv)
totalServer = encodeFileHandler :<|> decodeFileHandler
