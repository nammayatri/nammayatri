module API.Swagger.Types where

import Beckn.Utils.Servant.HTML
import qualified Data.ByteString as BS
import Data.OpenApi (OpenApi)
import Servant

type API =
  SwaggerAPI
    :<|> OpenAPI

type SwaggerAPI = "swagger" :> Get '[HTML] BS.ByteString

type OpenAPI = "openapi" :> Get '[JSON] OpenApi
