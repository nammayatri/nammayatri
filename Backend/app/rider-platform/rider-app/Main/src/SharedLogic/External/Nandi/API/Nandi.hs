module SharedLogic.External.Nandi.API.Nandi where

import qualified EulerHS.Types as ET
import Kernel.Prelude
import Servant
import SharedLogic.External.Nandi.Types

type NandiPatternsAPI = "otp" :> "routers" :> "default" :> "index" :> "patterns" :> Get '[JSON] NandiPatternsRes

type NandiGetSpecificPatternAPI = "otp" :> "routers" :> "default" :> "index" :> "patterns" :> Capture "patternId" Text :> Get '[JSON] NandiPatternDetails

type RoutesAPI = "otp" :> "routers" :> "default" :> "index" :> "routes" :> Get '[JSON] [NandiRoutesRes]

nandiPatternsAPI :: Proxy NandiPatternsAPI
nandiPatternsAPI = Proxy

nandiGetSpecificPatternAPI :: Proxy NandiGetSpecificPatternAPI
nandiGetSpecificPatternAPI = Proxy

nandiRoutesAPI :: Proxy RoutesAPI
nandiRoutesAPI = Proxy

getNandiPatterns :: ET.EulerClient NandiPatternsRes
getNandiPatterns = ET.client nandiPatternsAPI

getNandiGetSpecificPattern :: Text -> ET.EulerClient NandiPatternDetails
getNandiGetSpecificPattern = ET.client nandiGetSpecificPatternAPI

getNandiRoutes :: ET.EulerClient [NandiRoutesRes]
getNandiRoutes = ET.client nandiRoutesAPI
