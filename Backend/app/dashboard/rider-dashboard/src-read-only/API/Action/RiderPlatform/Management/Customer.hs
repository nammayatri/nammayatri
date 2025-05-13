{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Customer
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Customer
import qualified Dashboard.Common
import qualified Domain.Action.RiderPlatform.Management.Customer
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("customer" :> (GetCustomerList :<|> DeleteCustomerDelete :<|> PostCustomerBlock :<|> PostCustomerUnblock :<|> GetCustomerInfo :<|> PostCustomerCancellationDuesSync :<|> GetCustomerCancellationDuesDetails :<|> PostCustomerUpdateSafetyCenterBlocking :<|> PostCustomerPersonNumbers :<|> PostCustomerPersonId))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getCustomerList merchantId city :<|> deleteCustomerDelete merchantId city :<|> postCustomerBlock merchantId city :<|> postCustomerUnblock merchantId city :<|> getCustomerInfo merchantId city :<|> postCustomerCancellationDuesSync merchantId city :<|> getCustomerCancellationDuesDetails merchantId city :<|> postCustomerUpdateSafetyCenterBlocking merchantId city :<|> postCustomerPersonNumbers merchantId city :<|> postCustomerPersonId merchantId city

type GetCustomerList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.CUSTOMER / 'API.Types.RiderPlatform.Management.Customer.GET_CUSTOMER_LIST)
      :> API.Types.RiderPlatform.Management.Customer.GetCustomerList
  )

type DeleteCustomerDelete =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.CUSTOMER / 'API.Types.RiderPlatform.Management.Customer.DELETE_CUSTOMER_DELETE)
      :> API.Types.RiderPlatform.Management.Customer.DeleteCustomerDelete
  )

type PostCustomerBlock =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.CUSTOMER / 'API.Types.RiderPlatform.Management.Customer.POST_CUSTOMER_BLOCK)
      :> API.Types.RiderPlatform.Management.Customer.PostCustomerBlock
  )

type PostCustomerUnblock =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.CUSTOMER / 'API.Types.RiderPlatform.Management.Customer.POST_CUSTOMER_UNBLOCK)
      :> API.Types.RiderPlatform.Management.Customer.PostCustomerUnblock
  )

type GetCustomerInfo =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.CUSTOMER / 'API.Types.RiderPlatform.Management.Customer.GET_CUSTOMER_INFO)
      :> API.Types.RiderPlatform.Management.Customer.GetCustomerInfo
  )

type PostCustomerCancellationDuesSync =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.CUSTOMER / 'API.Types.RiderPlatform.Management.Customer.POST_CUSTOMER_CANCELLATION_DUES_SYNC)
      :> API.Types.RiderPlatform.Management.Customer.PostCustomerCancellationDuesSync
  )

type GetCustomerCancellationDuesDetails =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.CUSTOMER / 'API.Types.RiderPlatform.Management.Customer.GET_CUSTOMER_CANCELLATION_DUES_DETAILS)
      :> API.Types.RiderPlatform.Management.Customer.GetCustomerCancellationDuesDetails
  )

type PostCustomerUpdateSafetyCenterBlocking =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.CUSTOMER / 'API.Types.RiderPlatform.Management.Customer.POST_CUSTOMER_UPDATE_SAFETY_CENTER_BLOCKING)
      :> API.Types.RiderPlatform.Management.Customer.PostCustomerUpdateSafetyCenterBlocking
  )

type PostCustomerPersonNumbers =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.CUSTOMER / 'API.Types.RiderPlatform.Management.Customer.POST_CUSTOMER_PERSON_NUMBERS)
      :> API.Types.RiderPlatform.Management.Customer.PostCustomerPersonNumbers
  )

type PostCustomerPersonId =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.CUSTOMER / 'API.Types.RiderPlatform.Management.Customer.POST_CUSTOMER_PERSON_ID)
      :> API.Types.RiderPlatform.Management.Customer.PostCustomerPersonId
  )

getCustomerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Customer) -> Environment.FlowHandler API.Types.RiderPlatform.Management.Customer.CustomerListRes)
getCustomerList merchantShortId opCity apiTokenInfo limit offset enabled blocked phone personId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Customer.getCustomerList merchantShortId opCity apiTokenInfo limit offset enabled blocked phone personId

deleteCustomerDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteCustomerDelete merchantShortId opCity apiTokenInfo customerId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Customer.deleteCustomerDelete merchantShortId opCity apiTokenInfo customerId

postCustomerBlock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerBlock merchantShortId opCity apiTokenInfo customerId blockReason = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Customer.postCustomerBlock merchantShortId opCity apiTokenInfo customerId blockReason

postCustomerUnblock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerUnblock merchantShortId opCity apiTokenInfo customerId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Customer.postCustomerUnblock merchantShortId opCity apiTokenInfo customerId

getCustomerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler API.Types.RiderPlatform.Management.Customer.CustomerInfoRes)
getCustomerInfo merchantShortId opCity apiTokenInfo customerId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Customer.getCustomerInfo merchantShortId opCity apiTokenInfo customerId

postCustomerCancellationDuesSync :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Customer -> API.Types.RiderPlatform.Management.Customer.CustomerCancellationDuesSyncReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerCancellationDuesSync merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Customer.postCustomerCancellationDuesSync merchantShortId opCity apiTokenInfo customerId req

getCustomerCancellationDuesDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler API.Types.RiderPlatform.Management.Customer.CancellationDuesDetailsRes)
getCustomerCancellationDuesDetails merchantShortId opCity apiTokenInfo customerId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Customer.getCustomerCancellationDuesDetails merchantShortId opCity apiTokenInfo customerId

postCustomerUpdateSafetyCenterBlocking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Customer -> API.Types.RiderPlatform.Management.Customer.UpdateSafetyCenterBlockingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerUpdateSafetyCenterBlocking merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Customer.postCustomerUpdateSafetyCenterBlocking merchantShortId opCity apiTokenInfo customerId req

postCustomerPersonNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.PersonIdsReq -> Environment.FlowHandler [Dashboard.Common.PersonRes])
postCustomerPersonNumbers merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Customer.postCustomerPersonNumbers merchantShortId opCity apiTokenInfo req

postCustomerPersonId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.PersonMobileNoReq -> Environment.FlowHandler [Dashboard.Common.PersonRes])
postCustomerPersonId merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Customer.postCustomerPersonId merchantShortId opCity apiTokenInfo req
