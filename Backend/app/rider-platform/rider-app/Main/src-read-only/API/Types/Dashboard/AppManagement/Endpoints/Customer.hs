{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Customer where

import qualified "this" API.Types.UI.Sos
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("customer" :> PostCustomerSosCreate)

type PostCustomerSosCreate =
  ( Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "sos" :> "create" :> ReqBody ('[JSON]) API.Types.UI.Sos.SosReq
      :> Post
           ('[JSON])
           API.Types.UI.Sos.SosRes
  )

newtype CustomerAPIs = CustomerAPIs {postCustomerSosCreate :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.Sos.SosReq -> EulerHS.Types.EulerClient API.Types.UI.Sos.SosRes)}

mkCustomerAPIs :: (Client EulerHS.Types.EulerClient API -> CustomerAPIs)
mkCustomerAPIs customerClient = (CustomerAPIs {..})
  where
    postCustomerSosCreate = customerClient

data CustomerUserActionType
  = POST_CUSTOMER_SOS_CREATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON CustomerUserActionType where
  toJSON (POST_CUSTOMER_SOS_CREATE) = Data.Aeson.String "POST_CUSTOMER_SOS_CREATE"

instance FromJSON CustomerUserActionType where
  parseJSON (Data.Aeson.String "POST_CUSTOMER_SOS_CREATE") = pure POST_CUSTOMER_SOS_CREATE
  parseJSON _ = fail "POST_CUSTOMER_SOS_CREATE expected"

$(Data.Singletons.TH.genSingletons [(''CustomerUserActionType)])
