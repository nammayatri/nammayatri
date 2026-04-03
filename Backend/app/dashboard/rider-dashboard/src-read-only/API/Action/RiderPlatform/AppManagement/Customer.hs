{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.RiderPlatform.AppManagement.Customer 
( API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth.Api
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import qualified "rider-app" API.Types.Dashboard.AppManagement.Customer
import qualified API.Types.Dashboard.AppManagement
import qualified Domain.Action.RiderPlatform.AppManagement.Customer
import qualified "rider-app" Domain.Types.Person
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified "rider-app" API.Types.UI.Sos
import qualified "rider-app" API.Types.UI.DeletedPerson
import qualified Kernel.Types.APISuccess
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment


type API = ("customer" :> (PostCustomerSosCreate :<|> PostCustomerDeletedPerson :<|> GetCustomerSavedLocations :<|> PostCustomerSavedLocations :<|> DeleteCustomerSavedLocations))
handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postCustomerSosCreate merchantId city :<|> postCustomerDeletedPerson merchantId city :<|> getCustomerSavedLocations merchantId city :<|> postCustomerSavedLocations merchantId city :<|> deleteCustomerSavedLocations merchantId city
type PostCustomerSosCreate = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                                      ('DSL)
                                      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.CUSTOMER) / ('API.Types.Dashboard.AppManagement.Customer.POST_CUSTOMER_SOS_CREATE)) :> API.Types.Dashboard.AppManagement.Customer.PostCustomerSosCreate)
type PostCustomerDeletedPerson = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                                          ('DSL)
                                          (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.CUSTOMER) / ('API.Types.Dashboard.AppManagement.Customer.POST_CUSTOMER_DELETED_PERSON)) :> API.Types.Dashboard.AppManagement.Customer.PostCustomerDeletedPerson)
type GetCustomerSavedLocations = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                                          ('DSL)
                                          (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.CUSTOMER) / ('API.Types.Dashboard.AppManagement.Customer.GET_CUSTOMER_SAVED_LOCATIONS)) :> API.Types.Dashboard.AppManagement.Customer.GetCustomerSavedLocations)
type PostCustomerSavedLocations = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                                           ('DSL)
                                           (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.CUSTOMER) / ('API.Types.Dashboard.AppManagement.Customer.POST_CUSTOMER_SAVED_LOCATIONS)) :> API.Types.Dashboard.AppManagement.Customer.PostCustomerSavedLocations)
type DeleteCustomerSavedLocations = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                                             ('DSL)
                                             (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.CUSTOMER) / ('API.Types.Dashboard.AppManagement.Customer.DELETE_CUSTOMER_SAVED_LOCATIONS)) :> API.Types.Dashboard.AppManagement.Customer.DeleteCustomerSavedLocations)
postCustomerSosCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.Sos.SosReq -> Environment.FlowHandler API.Types.UI.Sos.SosRes)
postCustomerSosCreate merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Customer.postCustomerSosCreate merchantShortId opCity apiTokenInfo customerId req
postCustomerDeletedPerson :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.DeletedPerson.DeletedPersonReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerDeletedPerson merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Customer.postCustomerDeletedPerson merchantShortId opCity apiTokenInfo customerId req
getCustomerSavedLocations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Customer.SavedReqLocationsListRes)
getCustomerSavedLocations merchantShortId opCity apiTokenInfo customerId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Customer.getCustomerSavedLocations merchantShortId opCity apiTokenInfo customerId
postCustomerSavedLocations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.AppManagement.Customer.CreateSavedReqLocationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerSavedLocations merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Customer.postCustomerSavedLocations merchantShortId opCity apiTokenInfo customerId req
deleteCustomerSavedLocations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteCustomerSavedLocations merchantShortId opCity apiTokenInfo customerId tag = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Customer.deleteCustomerSavedLocations merchantShortId opCity apiTokenInfo customerId tag



