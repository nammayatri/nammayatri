{-# LANGUAGE AllowAmbiguousTypes #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.BharatTaxi.User
  ( API,
    handler,
  )
where

import Data.Aeson (Value)
import qualified Domain.Types.Role as DRole
import "lib-dashboard" Domain.Types.ServerName as DSN
import "lib-dashboard" Environment
import qualified EulerHS.Types as ET
import Kernel.Prelude hiding (toList)
import Kernel.Utils.Common (withFlowHandlerAPI')
import Servant
import Tools.Auth.Dashboard (DashboardAuth, TokenInfo)
import "lib-dashboard" Tools.Client as Client

data EstimateRequest = EstimateRequest
  { from_id :: Text,
    to_id :: Text,
    ride_type :: Text,
    rider_id :: Text,
    luggage :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

type ExternalFromListAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "from-list"
    :> Header "accept" Text
    :> Header "token" Text
    :> Get '[JSON] Value

type ExternalToListAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "to-list"
    :> Header "accept" Text
    :> Header "token" Text
    :> Get '[JSON] Value

type ExternalEstimateAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "estimate"
    :> Header "accept" Text
    :> Header "token" Text
    :> ReqBody '[JSON] EstimateRequest
    :> Post '[JSON] Value

type ExternalBookingAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "booking"
    :> QueryParam' '[Required] "estimateId" Text
    :> Header "accept" Text
    :> Header "token" Text
    :> Post '[JSON] Value

type API =
  "bharatTaxi"
    :> DashboardAuth 'DRole.DASHBOARD_ADMIN
    :> ( "fromList"
           :> Get '[JSON] Value
           :<|> "toList"
           :> Get '[JSON] Value
           :<|> "estimate"
           :> ReqBody '[JSON] EstimateRequest
           :> Post '[JSON] Value
           :<|> "booking"
           :> QueryParam' '[Required] "estimateId" Text
           :> Post '[JSON] Value
       )

handler :: FlowServer API
handler tokenInfo =
  fromList tokenInfo
    :<|> toList tokenInfo
    :<|> estimate tokenInfo
    :<|> booking tokenInfo

externalFromListAPI :: Proxy ExternalFromListAPI
externalFromListAPI = Proxy

externalFromListClient :: Maybe Text -> Maybe Text -> ET.EulerClient Value
externalFromListClient = ET.client externalFromListAPI

externalToListAPI :: Proxy ExternalToListAPI
externalToListAPI = Proxy

externalToListClient :: Maybe Text -> Maybe Text -> ET.EulerClient Value
externalToListClient = ET.client externalToListAPI

externalEstimateAPI :: Proxy ExternalEstimateAPI
externalEstimateAPI = Proxy

externalEstimateClient :: Maybe Text -> Maybe Text -> EstimateRequest -> ET.EulerClient Value
externalEstimateClient = ET.client externalEstimateAPI

externalBookingAPI :: Proxy ExternalBookingAPI
externalBookingAPI = Proxy

externalBookingClient :: Text -> Maybe Text -> Maybe Text -> ET.EulerClient Value
externalBookingClient = ET.client externalBookingAPI

-- | Bharat Taxi API DSL used with generic dashboard client.
data BharatTaxiAPIs = BharatTaxiAPIs
  { fromListDSL :: ET.EulerClient Value,
    toListDSL :: ET.EulerClient Value,
    estimateDSL :: EstimateRequest -> ET.EulerClient Value,
    bookingDSL :: Text -> ET.EulerClient Value
  }

-- | Build BharatTaxiAPIs from the configured data-server token.
mkBharatTaxiAPIs :: Text -> BharatTaxiAPIs
mkBharatTaxiAPIs token =
  let fromListDSL = externalFromListClient (Just "application/json") (Just token)
      toListDSL = externalToListClient (Just "application/json") (Just token)
      estimateDSL = externalEstimateClient (Just "application/json") (Just token)
      bookingDSL estimateId = externalBookingClient estimateId (Just "application/json") (Just token)
   in BharatTaxiAPIs {..}

-- | Generic caller for Bharat Taxi service, similar to callFleetAPI.
callBharatTaxiAPI ::
  forall m r b c.
  Client.DashboardClient BharatTaxiAPIs m r b c =>
  ((BharatTaxiAPIs -> b) -> c)
callBharatTaxiAPI =
  Client.callServerAPI @_ @m @r DSN.BHARAT_TAXI mkBharatTaxiAPIs "callBharatTaxiAPI"

fromList :: TokenInfo -> FlowHandler Value
fromList _ = withFlowHandlerAPI' $ callBharatTaxiAPI (.fromListDSL)

toList :: TokenInfo -> FlowHandler Value
toList _ = withFlowHandlerAPI' $ callBharatTaxiAPI (.toListDSL)

estimate :: TokenInfo -> EstimateRequest -> FlowHandler Value
estimate _ req = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.estimateDSL req)

booking :: TokenInfo -> Text -> FlowHandler Value
booking _ estimateId = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.bookingDSL estimateId)
