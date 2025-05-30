module SharedLogic.External.Nandi.API.Nandi where

import qualified EulerHS.Types as ET
import Kernel.Prelude
import Servant
import SharedLogic.External.Nandi.Types

type NandiPatternsAPI = "patterns" :> Capture "gtfs_id" Text :> Get '[JSON] [NandiPattern]

type NandiGetSpecificPatternAPI = "patterns" :> Capture "patternId" Text :> Get '[JSON] NandiPatternDetails

type RoutesAPI = "routes" :> Capture "gtfs_id" Text :> Get '[JSON] [NandiRoutesRes]

nandiPatternsAPI :: Proxy NandiPatternsAPI
nandiPatternsAPI = Proxy

nandiGetSpecificPatternAPI :: Proxy NandiGetSpecificPatternAPI
nandiGetSpecificPatternAPI = Proxy

nandiRoutesAPI :: Proxy RoutesAPI
nandiRoutesAPI = Proxy

getNandiPatterns :: Text -> ET.EulerClient [NandiPattern]
getNandiPatterns = ET.client nandiPatternsAPI

getNandiGetSpecificPattern :: Text -> ET.EulerClient NandiPatternDetails
getNandiGetSpecificPattern = ET.client nandiGetSpecificPatternAPI

getNandiRoutes :: Text -> ET.EulerClient [NandiRoutesRes]
getNandiRoutes = ET.client nandiRoutesAPI
