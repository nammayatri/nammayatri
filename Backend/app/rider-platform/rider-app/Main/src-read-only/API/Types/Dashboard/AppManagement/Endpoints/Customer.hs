{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.Dashboard.AppManagement.Endpoints.Customer where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Kernel.Prelude
import qualified "this" Domain.Types.SavedReqLocation
import qualified Kernel.Types.Id
import qualified "this" Domain.Types.Person
import qualified "this" API.Types.UI.Sos
import qualified "this" API.Types.UI.DeletedPerson
import qualified Kernel.Types.APISuccess
import qualified EulerHS.Types
import qualified Data.Singletons.TH



data CreateSavedReqLocationReq
    = CreateSavedReqLocationReq {area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                 areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                 building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                 city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                 country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                 door :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                 isMoved :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                                 lat :: Kernel.Prelude.Double,
                                 locationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                 lon :: Kernel.Prelude.Double,
                                 placeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                 state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                 street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                 tag :: Kernel.Prelude.Text,
                                 ward :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data SavedReqLocationsListRes
    = SavedReqLocationsListRes {list :: [Domain.Types.SavedReqLocation.SavedReqLocationAPIEntity]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
type API = ("customer" :> (PostCustomerSosCreate :<|> PostCustomerDeletedPerson :<|> GetCustomerSavedLocations :<|> PostCustomerSavedLocations :<|> DeleteCustomerSavedLocations))
type PostCustomerSosCreate = (Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "sos" :> "create" :> ReqBody ('[JSON]) API.Types.UI.Sos.SosReq :> Post ('[JSON])
                                                                                                                                                                             API.Types.UI.Sos.SosRes)
type PostCustomerDeletedPerson = (Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "deleted" :> "person" :> ReqBody ('[JSON])
                                                                                                                                           API.Types.UI.DeletedPerson.DeletedPersonReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)
type GetCustomerSavedLocations = (Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "savedLocations" :> Get ('[JSON]) SavedReqLocationsListRes)
type PostCustomerSavedLocations = (Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "savedLocations" :> ReqBody ('[JSON]) CreateSavedReqLocationReq :> Post ('[JSON])
                                                                                                                                                                                   Kernel.Types.APISuccess.APISuccess)
type DeleteCustomerSavedLocations = (Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "savedLocations" :> Capture "tag" Kernel.Prelude.Text :> Delete ('[JSON])
                                                                                                                                                                             Kernel.Types.APISuccess.APISuccess)
data CustomerAPIs
    = CustomerAPIs {postCustomerSosCreate :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.Sos.SosReq -> EulerHS.Types.EulerClient API.Types.UI.Sos.SosRes),
                    postCustomerDeletedPerson :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.DeletedPerson.DeletedPersonReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
                    getCustomerSavedLocations :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> EulerHS.Types.EulerClient SavedReqLocationsListRes),
                    postCustomerSavedLocations :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> CreateSavedReqLocationReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
                    deleteCustomerSavedLocations :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}
mkCustomerAPIs :: (Client EulerHS.Types.EulerClient API -> CustomerAPIs)
mkCustomerAPIs customerClient = (CustomerAPIs {..})
                   where postCustomerSosCreate :<|> postCustomerDeletedPerson :<|> getCustomerSavedLocations :<|> postCustomerSavedLocations :<|> deleteCustomerSavedLocations = customerClient
data CustomerUserActionType
    = POST_CUSTOMER_SOS_CREATE | POST_CUSTOMER_DELETED_PERSON | GET_CUSTOMER_SAVED_LOCATIONS | POST_CUSTOMER_SAVED_LOCATIONS | DELETE_CUSTOMER_SAVED_LOCATIONS
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''CustomerUserActionType)])

