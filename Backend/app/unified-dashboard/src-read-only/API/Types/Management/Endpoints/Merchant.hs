{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Management.Endpoints.Merchant where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Role
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Verification.SafetyPortal.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data ChangeMerchantEnableStateReq = ChangeMerchantEnableStateReq {enable :: Kernel.Prelude.Bool, merchantShortId :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateMerchantReq = CreateMerchantReq
  { defaultOperatingCity :: Kernel.Types.Beckn.Context.City,
    domain :: Data.Text.Text,
    is2faMandatory :: Kernel.Prelude.Bool,
    shortId :: Data.Text.Text,
    supportedOperatingCities :: [Kernel.Types.Beckn.Context.City],
    website :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateMerchantWithAdminReq = CreateMerchantWithAdminReq
  { adminEmail :: Data.Text.Text,
    adminFirstName :: Data.Text.Text,
    adminLastName :: Data.Text.Text,
    adminMobileCountryCode :: Data.Text.Text,
    adminMobileNumber :: Data.Text.Text,
    adminPassword :: Data.Text.Text,
    defaultOperatingCity :: Kernel.Types.Beckn.Context.City,
    domain :: Data.Text.Text,
    is2faMandatory :: Kernel.Prelude.Bool,
    shortId :: Data.Text.Text,
    supportedOperatingCities :: [Kernel.Types.Beckn.Context.City],
    website :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ListMerchantResp = ListMerchantResp {list :: [MerchantAPIEntity], summary :: Kernel.External.Verification.SafetyPortal.Types.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MerchantAPIEntity = MerchantAPIEntity
  { adminList :: [(Text, Data.Text.Text)],
    authToken :: Kernel.Prelude.Maybe Data.Text.Text,
    defaultOperatingCity :: Kernel.Types.Beckn.Context.City,
    domain :: Kernel.Prelude.Maybe Data.Text.Text,
    enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
    supportedOperatingCities :: [Kernel.Types.Beckn.Context.City],
    website :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PersonAPIEntity = PersonAPIEntity
  { availableCitiesForMerchant :: Kernel.Prelude.Maybe [Domain.Types.Merchant.AvailableCitiesForMerchant],
    availableMerchants :: [Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant],
    email :: Kernel.Prelude.Maybe Data.Text.Text,
    firstName :: Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    lastName :: Data.Text.Text,
    mobileCountryCode :: Data.Text.Text,
    mobileNumber :: Data.Text.Text,
    registeredAt :: Kernel.Prelude.UTCTime,
    role :: Domain.Types.Role.RoleAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("/merchant" :> (CreateMerchantWithAdmin :<|> CreateMerchant :<|> ListMerchants :<|> ChangeMerchantEnableState))

type CreateMerchantWithAdmin = ("create" :> "withAdmin" :> ReqBody ('[JSON]) CreateMerchantWithAdminReq :> Post ('[JSON]) PersonAPIEntity)

type CreateMerchant = ("create" :> ReqBody ('[JSON]) CreateMerchantReq :> Post ('[JSON]) MerchantAPIEntity)

type ListMerchants = ("list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "shortId" Data.Text.Text :> Get ('[JSON]) ListMerchantResp)

type ChangeMerchantEnableState = ("change" :> "enableState" :> ReqBody ('[JSON]) ChangeMerchantEnableStateReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data MerchantAPIs = MerchantAPIs
  { createMerchantWithAdmin :: (CreateMerchantWithAdminReq -> EulerHS.Types.EulerClient PersonAPIEntity),
    createMerchant :: (CreateMerchantReq -> EulerHS.Types.EulerClient MerchantAPIEntity),
    listMerchants :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Data.Text.Text) -> EulerHS.Types.EulerClient ListMerchantResp),
    changeMerchantEnableState :: (ChangeMerchantEnableStateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkMerchantAPIs :: (Client EulerHS.Types.EulerClient API -> MerchantAPIs)
mkMerchantAPIs merchantClient = (MerchantAPIs {..})
  where
    createMerchantWithAdmin :<|> createMerchant :<|> listMerchants :<|> changeMerchantEnableState = merchantClient

data MerchantUserActionType
  = CREATE_MERCHANT_WITH_ADMIN
  | CREATE_MERCHANT
  | LIST_MERCHANTS
  | CHANGE_MERCHANT_ENABLE_STATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''MerchantUserActionType)])
