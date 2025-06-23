{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.Customer
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.Customer
import qualified "rider-app" API.Types.UI.DeletedPerson
import qualified "rider-app" API.Types.UI.Sos
import qualified Domain.Action.RiderPlatform.AppManagement.Customer
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("customer" :> (PostCustomerSosCreate :<|> PostCustomerDeletedPerson))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postCustomerSosCreate merchantId city :<|> postCustomerDeletedPerson merchantId city

type PostCustomerSosCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.CUSTOMER / 'API.Types.Dashboard.AppManagement.Customer.POST_CUSTOMER_SOS_CREATE)
      :> API.Types.Dashboard.AppManagement.Customer.PostCustomerSosCreate
  )

type PostCustomerDeletedPerson =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.CUSTOMER / 'API.Types.Dashboard.AppManagement.Customer.POST_CUSTOMER_DELETED_PERSON)
      :> API.Types.Dashboard.AppManagement.Customer.PostCustomerDeletedPerson
  )

postCustomerSosCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.Sos.SosReq -> Environment.FlowHandler API.Types.UI.Sos.SosRes)
postCustomerSosCreate merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Customer.postCustomerSosCreate merchantShortId opCity apiTokenInfo customerId req

postCustomerDeletedPerson :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.DeletedPerson.DeletedPersonReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerDeletedPerson merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Customer.postCustomerDeletedPerson merchantShortId opCity apiTokenInfo customerId req
