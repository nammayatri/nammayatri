{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CallInternalMLPricing where

import qualified Data.HashMap.Strict as HM
import Domain.Types.Common
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.MerchantOperatingCity as DMOC
import EulerHS.Types (EulerClient, client)
import Kernel.External.Maps.Types
import Kernel.External.Slack.Types
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common hiding (Error)
import Kernel.Utils.Dhall (FromDhall)
import qualified Kernel.Utils.Servant.Client as EC
import Servant hiding (throwError)
import Tools.Metrics (CoreMetrics)

data GetCongestionChargeReq = GetCongestionChargeReq
  { txnId :: Maybe Text,
    fromLocation :: LatLong,
    toLocation :: Maybe LatLong,
    bookingTime :: Maybe UTCTime,
    distance :: Meters,
    duration :: Maybe Seconds,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    serviceTier :: ServiceTierType,
    fare :: Maybe HighPrecMoney,
    baseFare :: HighPrecMoney,
    distanceFare :: Maybe HighPrecMoney,
    pickupFare :: HighPrecMoney,
    maxDAFare :: Maybe HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data GetCongestionChargeRes = GetCongestionChargeRes
  { congestionChargeMultiplier :: Maybe FarePolicyD.CongestionChargeMultiplier
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MLPricingInternal = MLPricingInternal
  { name :: Text,
    url :: BaseUrl,
    apiKey :: Text,
    internalKey :: Text
  }
  deriving (Generic, FromDhall)

type GetCongestionChargeAPI =
  "internal"
    :> "getCongestionCharge"
    :> Header "token" Text
    :> ReqBody '[JSON] GetCongestionChargeReq
    :> Post '[JSON] GetCongestionChargeRes

getCongestionChargeClient :: Maybe Text -> GetCongestionChargeReq -> EulerClient GetCongestionChargeRes
getCongestionChargeClient = client (Proxy @GetCongestionChargeAPI)

getCongestionChargeApi :: Proxy GetCongestionChargeAPI
getCongestionChargeApi = Proxy

getCongestionCharge ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  Text ->
  BaseUrl ->
  GetCongestionChargeReq ->
  m GetCongestionChargeRes
getCongestionCharge apiKey internalUrl request = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "ML_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (getCongestionChargeClient (Just apiKey) request) "GetCongestionChargeML" getCongestionChargeApi
