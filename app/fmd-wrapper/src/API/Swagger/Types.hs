module API.Swagger.Types where

import Data.OpenApi (OpenApi)
import Servant

type API = "swagger" :> Get '[JSON] OpenApi
