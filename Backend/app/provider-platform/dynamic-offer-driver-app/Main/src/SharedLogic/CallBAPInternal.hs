{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CallBAPInternal where

import qualified Data.HashMap as HM
import Domain.Types.Ride as DRide
import EulerHS.Types (EulerClient, client)
import Kernel.External.Slack.Types
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id (Id)
import Kernel.Utils.Common hiding (Error)
import Kernel.Utils.Dhall (FromDhall)
import qualified Kernel.Utils.Servant.Client as EC
import Servant hiding (throwError)
import Tools.Metrics (CoreMetrics)

data FeedbackReq = FeedbackReq
  { rideId :: Id DRide.Ride,
    ratingValue :: Int,
    feedbackDetails :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data AppBackendBapInternal = AppBackendBapInternal
  { name :: Text,
    url :: BaseUrl,
    apiKey :: Text,
    internalKey :: Text
  }
  deriving (Generic, FromDhall)

type FeedBackAPI =
  "internal"
    :> "feedback"
    :> "rateRide"
    :> Header "token" Text
    :> ReqBody '[JSON] FeedbackReq
    :> Post '[JSON] APISuccess

feedbackClient :: Maybe Text -> FeedbackReq -> EulerClient APISuccess
feedbackClient = client (Proxy @FeedBackAPI)

feedbackApi :: Proxy FeedBackAPI
feedbackApi = Proxy

feedback ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  FeedbackReq ->
  m APISuccess
feedback apiKey internalUrl request = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BAP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (feedbackClient (Just apiKey) request) "FeedBack" feedbackApi
