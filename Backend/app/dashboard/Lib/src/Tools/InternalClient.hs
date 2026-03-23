{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.InternalClient (SendSMSReq (..), SendSMSRes (..), callBPPInternalSendSMS) where

import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Domain.Types.ServerName as DSN
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Utils.Common hiding (Error, callAPI, throwError)
import Kernel.Utils.Error.Throwing (throwError)
import Servant hiding (throwError)
import Tools.Error
import Tools.Metrics

data SendSMSReq = SendSMSReq
  { phoneNumber :: Text,
    messageKey :: Text,
    templateVars :: Map Text Text,
    isOtp :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype SendSMSRes = SendSMSRes
  { otp :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

type SendSMSAPI =
  "internal"
    :> "sendSMS"
    :> Capture "merchantShortId" Text
    :> Capture "city" City.City
    :> Header "api-key" Text
    :> ReqBody '[JSON] SendSMSReq
    :> Post '[JSON] SendSMSRes

sendSMSClient :: Text -> City.City -> Maybe Text -> SendSMSReq -> Euler.EulerClient SendSMSRes
sendSMSClient = Euler.client (Proxy @SendSMSAPI)

callBPPInternalSendSMS ::
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DSN.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  Text ->
  City.City ->
  SendSMSReq ->
  m SendSMSRes
callBPPInternalSendSMS merchantShortId city req = do
  dataServers <- asks (.dataServers)
  let mbDataServer = find (\s -> s.name == DSN.DRIVER_OFFER_BPP) dataServers
  dataServer <- maybe (throwError $ InternalError "DRIVER_OFFER_BPP data server not found") pure mbDataServer
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callApiUnwrappingApiError (identity @Error) Nothing Nothing (Just internalEndPointHashMap) dataServer.url (sendSMSClient merchantShortId city (Just dataServer.token) req) "callBPPInternalSendSMS" (Proxy :: Proxy Raw)
