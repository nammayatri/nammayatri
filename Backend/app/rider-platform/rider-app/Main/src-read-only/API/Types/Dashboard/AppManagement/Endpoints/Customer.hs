{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Customer where

import qualified "this" API.Types.UI.DeletedPerson
import qualified "this" API.Types.UI.Sos
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("customer" :> (PostCustomerSosCreate :<|> PostCustomerDeletedPerson))

type PostCustomerSosCreate =
  ( Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "sos" :> "create" :> ReqBody '[JSON] API.Types.UI.Sos.SosReq
      :> Post
           '[JSON]
           API.Types.UI.Sos.SosRes
  )

type PostCustomerDeletedPerson =
  ( Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "deleted" :> "person"
      :> ReqBody
           '[JSON]
           API.Types.UI.DeletedPerson.DeletedPersonReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

data CustomerAPIs = CustomerAPIs
  { postCustomerSosCreate :: Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.Sos.SosReq -> EulerHS.Types.EulerClient API.Types.UI.Sos.SosRes,
    postCustomerDeletedPerson :: Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.DeletedPerson.DeletedPersonReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkCustomerAPIs :: (Client EulerHS.Types.EulerClient API -> CustomerAPIs)
mkCustomerAPIs customerClient = (CustomerAPIs {..})
  where
    postCustomerSosCreate :<|> postCustomerDeletedPerson = customerClient

data CustomerUserActionType
  = POST_CUSTOMER_SOS_CREATE
  | POST_CUSTOMER_DELETED_PERSON
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''CustomerUserActionType])
