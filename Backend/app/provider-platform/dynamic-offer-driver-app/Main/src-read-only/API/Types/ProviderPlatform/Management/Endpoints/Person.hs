{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Person where

import qualified API.Types.ProviderPlatform.Management.Account
import qualified Dashboard.Common
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data AvailableCitiesForMerchant = AvailableCitiesForMerchant {merchantShortId :: Kernel.Types.Id.ShortId Dashboard.Common.Merchant, operatingCity :: [Kernel.Types.Beckn.Context.City]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ListPersonRes = ListPersonRes {list :: [PersonAPIEntity], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PersonAPIEntity = PersonAPIEntity
  { id :: Kernel.Types.Id.Id Dashboard.Common.Person,
    firstName :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    role :: API.Types.ProviderPlatform.Management.Account.RoleAPIEntity,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Text,
    availableMerchants :: [Kernel.Types.Id.ShortId Dashboard.Common.Merchant],
    availableCitiesForMerchant :: Kernel.Prelude.Maybe [AvailableCitiesForMerchant],
    registeredAt :: Kernel.Prelude.UTCTime,
    verified :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    receiveNotification :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    language :: Kernel.Prelude.Maybe Kernel.External.Types.Language
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("person" :> GetPersonList)

type GetPersonList = ("list" :> QueryParam "searchString" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get ('[JSON]) ListPersonRes)

newtype PersonAPIs = PersonAPIs {getPersonList :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient ListPersonRes)}

mkPersonAPIs :: (Client EulerHS.Types.EulerClient API -> PersonAPIs)
mkPersonAPIs personClient = (PersonAPIs {..})
  where
    getPersonList = personClient

data PersonUserActionType
  = GET_PERSON_LIST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON PersonUserActionType where
  toJSON (GET_PERSON_LIST) = Data.Aeson.String "GET_PERSON_LIST"

instance FromJSON PersonUserActionType where
  parseJSON (Data.Aeson.String "GET_PERSON_LIST") = pure GET_PERSON_LIST
  parseJSON _ = fail "GET_PERSON_LIST expected"

$(Data.Singletons.TH.genSingletons [(''PersonUserActionType)])
