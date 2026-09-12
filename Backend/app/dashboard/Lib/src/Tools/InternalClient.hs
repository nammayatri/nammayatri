{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Internal HTTP calls the dashboard makes to an application server.
--
-- Lives in lib-dashboard so the login/user-administration tree can be served by
-- an application server as well as by provider-dashboard. The one call that
-- needs an application package's request type
-- request and response bodies are declared here rather than imported from the
-- application package, so this module links no application code. They must stay
-- structurally identical to @API.Types.UnifiedDashboard.Management.Endpoints.Person@
-- in dynamic-offer-driver-app, which is what actually serves them.
module Tools.InternalClient
  ( SendSMSReq (..),
    SendSMSRes (..),
    callBPPInternalSendSMS,
    callBAPInternalSendSMS,
    SendEmailOTPReq (..),
    SendEmailOTPRes (..),
    callBPPInternalSendEmailOTP,
    callBAPInternalSendEmailOTP,
    VerifyEmailUpdateReq (..),
    callBPPInternalVerifyEmailUpdate,
    callBAPInternalVerifyEmailUpdate,
    CreatePersonReq (..),
    CreatePersonResp (..),
    callBPPInternalCreatePerson,
  )
where

import qualified Dashboard.Common as Common
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Domain.Types.ServerName as DSN
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id (Id)
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

callBAPInternalSendSMS ::
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DSN.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  Text ->
  City.City ->
  SendSMSReq ->
  m SendSMSRes
callBAPInternalSendSMS merchantShortId city req = do
  dataServers <- asks (.dataServers)
  let mbDataServer = find (\s -> s.name == DSN.APP_BACKEND) dataServers
  dataServer <- maybe (throwError $ InternalError "APP_BACKEND data server not found") pure mbDataServer
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callApiUnwrappingApiError (identity @Error) Nothing Nothing (Just internalEndPointHashMap) dataServer.url (sendSMSClient merchantShortId city (Just dataServer.token) req) "callBAPInternalSendSMS" (Proxy :: Proxy Raw)

-- Email OTP

newtype SendEmailOTPReq = SendEmailOTPReq
  { email :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype SendEmailOTPRes = SendEmailOTPRes
  { otp :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

type SendEmailOTPAPI =
  "internal"
    :> "sendEmailOTP"
    :> Capture "merchantShortId" Text
    :> Capture "city" City.City
    :> Header "api-key" Text
    :> ReqBody '[JSON] SendEmailOTPReq
    :> Post '[JSON] SendEmailOTPRes

sendEmailOTPClient :: Text -> City.City -> Maybe Text -> SendEmailOTPReq -> Euler.EulerClient SendEmailOTPRes
sendEmailOTPClient = Euler.client (Proxy @SendEmailOTPAPI)

callBPPInternalSendEmailOTP ::
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DSN.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  Text ->
  City.City ->
  SendEmailOTPReq ->
  m SendEmailOTPRes
callBPPInternalSendEmailOTP merchantShortId city req = do
  dataServers <- asks (.dataServers)
  let mbDataServer = find (\s -> s.name == DSN.DRIVER_OFFER_BPP) dataServers
  dataServer <- maybe (throwError $ InternalError "DRIVER_OFFER_BPP data server not found") pure mbDataServer
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callApiUnwrappingApiError (identity @Error) Nothing Nothing (Just internalEndPointHashMap) dataServer.url (sendEmailOTPClient merchantShortId city (Just dataServer.token) req) "callBPPInternalSendEmailOTP" (Proxy :: Proxy Raw)

callBAPInternalSendEmailOTP ::
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DSN.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  Text ->
  City.City ->
  SendEmailOTPReq ->
  m SendEmailOTPRes
callBAPInternalSendEmailOTP merchantShortId city req = do
  dataServers <- asks (.dataServers)
  let mbDataServer = find (\s -> s.name == DSN.APP_BACKEND) dataServers
  dataServer <- maybe (throwError $ InternalError "APP_BACKEND data server not found") pure mbDataServer
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callApiUnwrappingApiError (identity @Error) Nothing Nothing (Just internalEndPointHashMap) dataServer.url (sendEmailOTPClient merchantShortId city (Just dataServer.token) req) "callBAPInternalSendEmailOTP" (Proxy :: Proxy Raw)

-- Verify Email Update

data VerifyEmailUpdateReq = VerifyEmailUpdateReq
  { email :: Text,
    requesteeId :: Text,
    requestorId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

type VerifyEmailUpdateAPI =
  "internal"
    :> "verifyEmailUpdate"
    :> Capture "merchantShortId" Text
    :> Header "api-key" Text
    :> ReqBody '[JSON] VerifyEmailUpdateReq
    :> Post '[JSON] APISuccess

verifyEmailUpdateClient :: Text -> Maybe Text -> VerifyEmailUpdateReq -> Euler.EulerClient APISuccess
verifyEmailUpdateClient = Euler.client (Proxy @VerifyEmailUpdateAPI)

callBPPInternalVerifyEmailUpdate ::
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DSN.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  Text ->
  VerifyEmailUpdateReq ->
  m APISuccess
callBPPInternalVerifyEmailUpdate merchantShortId req = do
  dataServers <- asks (.dataServers)
  let mbDataServer = find (\s -> s.name == DSN.DRIVER_OFFER_BPP) dataServers
  dataServer <- maybe (throwError $ InternalError "DRIVER_OFFER_BPP data server not found") pure mbDataServer
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callApiUnwrappingApiError (identity @Error) Nothing Nothing (Just internalEndPointHashMap) dataServer.url (verifyEmailUpdateClient merchantShortId (Just dataServer.token) req) "callBPPInternalVerifyEmailUpdate" (Proxy :: Proxy Raw)

callBAPInternalVerifyEmailUpdate ::
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DSN.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  Text ->
  VerifyEmailUpdateReq ->
  m APISuccess
callBAPInternalVerifyEmailUpdate merchantShortId req = do
  dataServers <- asks (.dataServers)
  let mbDataServer = find (\s -> s.name == DSN.APP_BACKEND) dataServers
  dataServer <- maybe (throwError $ InternalError "APP_BACKEND data server not found") pure mbDataServer
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callApiUnwrappingApiError (identity @Error) Nothing Nothing (Just internalEndPointHashMap) dataServer.url (verifyEmailUpdateClient merchantShortId (Just dataServer.token) req) "callBAPInternalVerifyEmailUpdate" (Proxy :: Proxy Raw)

-- | Mirror of driver-app's @CreatePersonReq@. Field names are the wire contract:
-- changing one here without changing it there silently breaks person creation.
data CreatePersonReq = CreatePersonReq
  { email :: Maybe Text,
    firstName :: Text,
    lastName :: Text,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    password :: Maybe Text,
    roleName :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype CreatePersonResp = CreatePersonResp {personId :: Id Common.Person}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type CreatePersonAPI =
  "internal"
    :> "person"
    :> "create"
    :> Capture "merchantShortId" Text
    :> Capture "city" City.City
    :> Header "api-key" Text
    :> ReqBody '[JSON] CreatePersonReq
    :> Post '[JSON] CreatePersonResp

createPersonClient :: Text -> City.City -> Maybe Text -> CreatePersonReq -> Euler.EulerClient CreatePersonResp
createPersonClient = Euler.client (Proxy @CreatePersonAPI)

callBPPInternalCreatePerson ::
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DSN.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  Text ->
  City.City ->
  CreatePersonReq ->
  m CreatePersonResp
callBPPInternalCreatePerson merchantShortId city req = do
  dataServers <- asks (.dataServers)
  let mbDataServer = find (\s -> s.name == DSN.DRIVER_OFFER_BPP) dataServers
  dataServer <- maybe (throwError $ InternalError "DRIVER_OFFER_BPP data server not found") pure mbDataServer
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callApiUnwrappingApiError (identity @Error) Nothing Nothing (Just internalEndPointHashMap) dataServer.url (createPersonClient merchantShortId city (Just dataServer.token) req) "callBPPInternalCreatePerson" (Proxy :: Proxy Raw)
