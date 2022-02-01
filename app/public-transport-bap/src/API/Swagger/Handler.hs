module API.Swagger.Handler where

import qualified API.Swagger.Types as Swagger
import API.Types (MainAPI)
import App.Types
import Beckn.Prelude
import Data.OpenApi
import Servant.OpenApi

swagger :: OpenApi
swagger = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "SWTD",
            _infoVersion = "1.0"
          }
    }

handler :: FlowServer Swagger.API
handler = return swagger