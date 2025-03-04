-- Enables automatic JSON serialization.
{-# LANGUAGE DeriveGeneric #-}
-- Allows string literals ("") to be treated as Text automatically
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Bus.CRUT.RouteSearch where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Bus.CRUT.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Servant
import Tools.Error

data RouteSearchReq = RouteSearchReq
  { city :: T.Text,
    searchString :: T.Text
  }
  deriving (Generic, Show)

data RouteSearchResult = RouteSearchResult
  { matchRatio :: Int,
    lastStopName :: T.Text,
    routeId :: T.Text,
    agencyName :: T.Text,
    routeName :: T.Text,
    transportType :: T.Text,
    serviceTypes :: [T.Text],
    firstStopName :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type RouteSearchAPI =
  "mticketing" :> "v2" :> "partner" :> "autocomplete" :> "mticket-routes"
    :> Header "Authorization" T.Text
    :> Header "userid" T.Text
    :> QueryParam "city" T.Text
    :> QueryParam "str" T.Text
    :> Get '[JSON] [RouteSearchResult]

routeSearchAPI :: Proxy RouteSearchAPI
routeSearchAPI = Proxy

getRouteSearchResults :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => EBIXConfig -> Text -> RouteSearchReq -> m [RouteSearchResult]
getRouteSearchResults config userId req = do
  authToken <- getAuthToken config userId
  response <-
    callAPI config.networkHostUrl
      (ET.client routeSearchAPI (Just $ "Basic " <> authToken) (Just userId) (Just req.city) (Just req.searchString))
      "getRouteSearchResults"
      routeSearchAPI
      >>= fromEitherM (ExternalAPICallError (Just "CRUT_ROUTE_SEARCH_API") config.networkHostUrl)
  return response

-- need to declare serviceTypes?
