{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Operator.Driver
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Operator
import qualified API.Types.ProviderPlatform.Operator.Driver
import qualified Dashboard.Common
import qualified Dashboard.ProviderPlatform.Fleet.Driver
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import qualified Domain.Action.ProviderPlatform.Operator.Driver
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("driver" :> (GetDriverOperatorFetchHubRequests :<|> PostDriverOperatorRespondHubRequest :<|> PostDriverOperatorSendJoiningOtp :<|> PostDriverOperatorVerifyJoiningOtp :<|> PostDriverOperatorAddDrivers))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverOperatorFetchHubRequests merchantId city :<|> postDriverOperatorRespondHubRequest merchantId city :<|> postDriverOperatorSendJoiningOtp merchantId city :<|> postDriverOperatorVerifyJoiningOtp merchantId city :<|> postDriverOperatorAddDrivers merchantId city

type GetDriverOperatorFetchHubRequests =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.DRIVER / 'API.Types.ProviderPlatform.Operator.Driver.GET_DRIVER_OPERATOR_FETCH_HUB_REQUESTS)
      :> API.Types.ProviderPlatform.Operator.Driver.GetDriverOperatorFetchHubRequests
  )

type PostDriverOperatorRespondHubRequest =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.DRIVER / 'API.Types.ProviderPlatform.Operator.Driver.POST_DRIVER_OPERATOR_RESPOND_HUB_REQUEST)
      :> API.Types.ProviderPlatform.Operator.Driver.PostDriverOperatorRespondHubRequest
  )

type PostDriverOperatorSendJoiningOtp =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.DRIVER / 'API.Types.ProviderPlatform.Operator.Driver.POST_DRIVER_OPERATOR_SEND_JOINING_OTP)
      :> API.Types.ProviderPlatform.Operator.Driver.PostDriverOperatorSendJoiningOtp
  )

type PostDriverOperatorVerifyJoiningOtp =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.DRIVER / 'API.Types.ProviderPlatform.Operator.Driver.POST_DRIVER_OPERATOR_VERIFY_JOINING_OTP)
      :> API.Types.ProviderPlatform.Operator.Driver.PostDriverOperatorVerifyJoiningOtp
  )

type PostDriverOperatorAddDrivers =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.DRIVER / 'API.Types.ProviderPlatform.Operator.Driver.POST_DRIVER_OPERATOR_ADD_DRIVERS)
      :> API.Types.ProviderPlatform.Operator.Driver.PostDriverOperatorAddDrivers
  )

getDriverOperatorFetchHubRequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Operator.Driver.RequestStatus -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Operator.Driver.RequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.OperationHub) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Driver.OperationHubReqResp)
getDriverOperatorFetchHubRequests merchantShortId opCity apiTokenInfo mbFrom mbTo mbStatus mbReqType mbLimit mbOffset mbDriverId mbMobileNumber mbOperationHubId mbRegistrationNo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Driver.getDriverOperatorFetchHubRequests merchantShortId opCity apiTokenInfo mbFrom mbTo mbStatus mbReqType mbLimit mbOffset mbDriverId mbMobileNumber mbOperationHubId mbRegistrationNo

postDriverOperatorRespondHubRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.Driver.RespondHubRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverOperatorRespondHubRequest merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Driver.postDriverOperatorRespondHubRequest merchantShortId opCity apiTokenInfo req

postDriverOperatorSendJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.FlowHandler Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverOperatorSendJoiningOtp merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Driver.postDriverOperatorSendJoiningOtp merchantShortId opCity apiTokenInfo req

postDriverOperatorVerifyJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.ProviderPlatform.Operator.Driver.VerifyOperatorJoiningOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverOperatorVerifyJoiningOtp merchantShortId opCity apiTokenInfo authId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Driver.postDriverOperatorVerifyJoiningOtp merchantShortId opCity apiTokenInfo authId req

postDriverOperatorAddDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.Driver.CreateDriversReq -> Environment.FlowHandler Dashboard.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverOperatorAddDrivers merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Driver.postDriverOperatorAddDrivers merchantShortId opCity apiTokenInfo req
