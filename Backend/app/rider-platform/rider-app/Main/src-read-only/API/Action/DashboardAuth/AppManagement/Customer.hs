{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.Customer
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Customer
import qualified "this" API.Types.UI.DeletedPerson
import qualified "this" API.Types.UI.Sos
import qualified Domain.Action.Dashboard.AppManagement.Customer
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("customer" :> (PostCustomerSosCreate :<|> PostCustomerDeletedPerson :<|> GetCustomerSavedLocations :<|> PostCustomerSavedLocations :<|> DeleteCustomerSavedLocations))

type PostCustomerSosCreate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/CUSTOMER/POST_CUSTOMER_SOS_CREATE" :> API.Types.Dashboard.AppManagement.Customer.PostCustomerSosCreate)

type PostCustomerDeletedPerson =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/CUSTOMER/POST_CUSTOMER_DELETED_PERSON"
      :> API.Types.Dashboard.AppManagement.Customer.PostCustomerDeletedPerson
  )

type GetCustomerSavedLocations =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/CUSTOMER/GET_CUSTOMER_SAVED_LOCATIONS"
      :> API.Types.Dashboard.AppManagement.Customer.GetCustomerSavedLocations
  )

type PostCustomerSavedLocations =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/CUSTOMER/POST_CUSTOMER_SAVED_LOCATIONS"
      :> API.Types.Dashboard.AppManagement.Customer.PostCustomerSavedLocations
  )

type DeleteCustomerSavedLocations =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/CUSTOMER/DELETE_CUSTOMER_SAVED_LOCATIONS"
      :> API.Types.Dashboard.AppManagement.Customer.DeleteCustomerSavedLocations
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postCustomerSosCreate merchantId city :<|> postCustomerDeletedPerson merchantId city :<|> getCustomerSavedLocations merchantId city :<|> postCustomerSavedLocations merchantId city :<|> deleteCustomerSavedLocations merchantId city

postCustomerSosCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.Sos.SosReq -> Environment.FlowHandler API.Types.UI.Sos.SosRes)
postCustomerSosCreate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Customer.postCustomerSosCreate a5 a4 a2 a1

postCustomerDeletedPerson :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.DeletedPerson.DeletedPersonReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerDeletedPerson a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Customer.postCustomerDeletedPerson a5 a4 a2 a1

getCustomerSavedLocations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Customer.SavedReqLocationsListRes)
getCustomerSavedLocations a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Customer.getCustomerSavedLocations a4 a3 a1

postCustomerSavedLocations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.AppManagement.Customer.CreateSavedReqLocationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerSavedLocations a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Customer.postCustomerSavedLocations a5 a4 a2 a1

deleteCustomerSavedLocations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteCustomerSavedLocations a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Customer.deleteCustomerSavedLocations a5 a4 a2 a1
