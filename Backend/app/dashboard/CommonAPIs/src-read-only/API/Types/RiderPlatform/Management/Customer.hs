{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Customer where

import qualified Dashboard.Common
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data CancellationDuesDetailsRes = CancellationDuesDetailsRes
  { cancellationDues :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    disputeChancesUsed :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    canBlockCustomer :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CustomerCancellationDuesSyncReq = CustomerCancellationDuesSyncReq
  { cancellationCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cancellationChargesWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    disputeChancesUsed :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    paymentMadeToDriver :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CustomerCancellationDuesSyncReq where
  hideSecrets = Kernel.Prelude.identity

data CustomerInfoRes = CustomerInfoRes
  { numberOfRides :: Kernel.Prelude.Int,
    falseSafetyAlarmCount :: Kernel.Prelude.Int,
    safetyCenterDisabledOnDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    totalSosCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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

data UpdateSafetyCenterBlockingReq = UpdateSafetyCenterBlockingReq {incrementCount :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, resetCount :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateSafetyCenterBlockingReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("customer" :> (GetCustomerList :<|> DeleteCustomerDelete :<|> PostCustomerBlock :<|> PostCustomerUnblock :<|> GetCustomerInfo :<|> PostCustomerCancellationDuesSync :<|> GetCustomerCancellationDuesDetails :<|> PostCustomerUpdateSafetyCenterBlocking :<|> PostCustomerPersonNumbers :<|> PostCustomerPersonId))

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

type DeleteCustomerDelete = (Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "delete" :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostCustomerBlock = (Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "block" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostCustomerUnblock = (Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "unblock" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetCustomerInfo = (Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "info" :> Get '[JSON] API.Types.RiderPlatform.Management.Customer.CustomerInfoRes)

type PostCustomerCancellationDuesSync =
  ( Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "cancellationDuesSync"
      :> ReqBody
           '[JSON]
           API.Types.RiderPlatform.Management.Customer.CustomerCancellationDuesSyncReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetCustomerCancellationDuesDetails =
  ( Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "getCancellationDuesDetails"
      :> Get
           '[JSON]
           API.Types.RiderPlatform.Management.Customer.CancellationDuesDetailsRes
  )

type PostCustomerUpdateSafetyCenterBlocking =
  ( Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "updateSafetyCenterBlocking"
      :> ReqBody
           '[JSON]
           API.Types.RiderPlatform.Management.Customer.UpdateSafetyCenterBlockingReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostCustomerPersonNumbers = ("personNumbers" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.PersonIdsReq :> Post '[JSON] [Dashboard.Common.PersonRes])

type PostCustomerPersonId = ("personId" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.PersonMobileNoReq :> Post '[JSON] [Dashboard.Common.PersonRes])

data CustomerAPIs = CustomerAPIs
  { getCustomerList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Customer) -> EulerHS.Types.EulerClient API.Types.RiderPlatform.Management.Customer.CustomerListRes,
    deleteCustomerDelete :: Kernel.Types.Id.Id Dashboard.Common.Customer -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postCustomerBlock :: Kernel.Types.Id.Id Dashboard.Common.Customer -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postCustomerUnblock :: Kernel.Types.Id.Id Dashboard.Common.Customer -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getCustomerInfo :: Kernel.Types.Id.Id Dashboard.Common.Customer -> EulerHS.Types.EulerClient API.Types.RiderPlatform.Management.Customer.CustomerInfoRes,
    postCustomerCancellationDuesSync :: Kernel.Types.Id.Id Dashboard.Common.Customer -> API.Types.RiderPlatform.Management.Customer.CustomerCancellationDuesSyncReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getCustomerCancellationDuesDetails :: Kernel.Types.Id.Id Dashboard.Common.Customer -> EulerHS.Types.EulerClient API.Types.RiderPlatform.Management.Customer.CancellationDuesDetailsRes,
    postCustomerUpdateSafetyCenterBlocking :: Kernel.Types.Id.Id Dashboard.Common.Customer -> API.Types.RiderPlatform.Management.Customer.UpdateSafetyCenterBlockingReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postCustomerPersonNumbers :: (Data.ByteString.Lazy.ByteString, Dashboard.Common.PersonIdsReq) -> EulerHS.Types.EulerClient [Dashboard.Common.PersonRes],
    postCustomerPersonId :: (Data.ByteString.Lazy.ByteString, Dashboard.Common.PersonMobileNoReq) -> EulerHS.Types.EulerClient [Dashboard.Common.PersonRes]
  }

mkCustomerAPIs :: (Client EulerHS.Types.EulerClient API -> CustomerAPIs)
mkCustomerAPIs customerClient = (CustomerAPIs {..})
  where
    getCustomerList :<|> deleteCustomerDelete :<|> postCustomerBlock :<|> postCustomerUnblock :<|> getCustomerInfo :<|> postCustomerCancellationDuesSync :<|> getCustomerCancellationDuesDetails :<|> postCustomerUpdateSafetyCenterBlocking :<|> postCustomerPersonNumbers :<|> postCustomerPersonId = customerClient

data CustomerEndpointDSL
  = GetCustomerListEndpoint
  | DeleteCustomerDeleteEndpoint
  | PostCustomerBlockEndpoint
  | PostCustomerUnblockEndpoint
  | GetCustomerInfoEndpoint
  | PostCustomerCancellationDuesSyncEndpoint
  | GetCustomerCancellationDuesDetailsEndpoint
  | PostCustomerUpdateSafetyCenterBlockingEndpoint
  | PostCustomerPersonNumbersEndpoint
  | PostCustomerPersonIdEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
