{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Customer where

import qualified Dashboard.Common
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data CustomerListItem = CustomerListItem
  { customerId :: Kernel.Types.Id.Id Dashboard.Common.Customer,
    firstName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    phoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    blocked :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CustomerListRes = CustomerListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, customers :: [API.Types.RiderPlatform.Management.Customer.CustomerListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("customer" :> GetCustomerList)

type GetCustomerList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "enabled" Kernel.Prelude.Bool
      :> QueryParam
           "blocked"
           Kernel.Prelude.Bool
      :> QueryParam "phone" Kernel.Prelude.Text
      :> QueryParam
           "personId"
           (Kernel.Types.Id.Id Dashboard.Common.Customer)
      :> Get
           '[JSON]
           API.Types.RiderPlatform.Management.Customer.CustomerListRes
  )

newtype CustomerAPIs = CustomerAPIs {getCustomerList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Customer) -> EulerHS.Types.EulerClient API.Types.RiderPlatform.Management.Customer.CustomerListRes}

mkCustomerAPIs :: (Client EulerHS.Types.EulerClient API -> CustomerAPIs)
mkCustomerAPIs customerClient = (CustomerAPIs {..})
  where
    getCustomerList = customerClient

data CustomerEndpointDSL
  = GetCustomerListEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON CustomerEndpointDSL where
  toJSON GetCustomerListEndpoint = Data.Aeson.String "GetCustomerListEndpoint"

instance FromJSON CustomerEndpointDSL where
  parseJSON (Data.Aeson.String "GetCustomerListEndpoint") = pure GetCustomerListEndpoint
  parseJSON _ = fail "GetCustomerListEndpoint expected"
