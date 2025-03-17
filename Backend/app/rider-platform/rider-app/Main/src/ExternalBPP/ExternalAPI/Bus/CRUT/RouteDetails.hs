{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Bus.CRUT.RouteDetails where

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

data RouteDetailsReq = RouteDetailsReq
  { city :: T.Text,
    routeId :: T.Text
  }
  deriving (Generic, Show)

data StopDetail = StopDetail
  { stopId :: T.Text,
    stopName :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data RouteDetailsRes = RouteDetailsRes
  { routeId :: T.Text,
    agencyName :: T.Text,
    routeName :: T.Text,
    mTicketEnabled :: Bool,
    transportType :: T.Text,
    stopSequenceWithDetails :: [StopDetail]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type RouteDetailsAPI =
  "mticketing" :> "v2" :> "partner" :> "route-details"
    :> Header "Authorization" T.Text
    :> Header "userid" T.Text
    :> QueryParam "city" T.Text
    :> QueryParam "routeId" T.Text
    :> Get '[JSON] RouteDetailsRes

routeDetailsAPI :: Proxy RouteDetailsAPI
routeDetailsAPI = Proxy

getRouteDetails :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => EBIXConfig -> Text -> RouteDetailsReq -> m RouteDetailsRes
getRouteDetails config userId req = do
  authToken <- getAuthToken config userId
  response <-
    callAPI config.networkHostUrl
      (ET.client routeDetailsAPI (Just $ "Basic " <> authToken) (Just userId) (Just req.city) (Just req.routeId))
      "getRouteDetails"
      routeDetailsAPI
      >>= fromEitherM (ExternalAPICallError (Just "CRUT_ROUTE_DETAILS_API") config.networkHostUrl)
  return response
