{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Account where

import qualified Dashboard.Common
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("account" :> PutAccountUpdateRole)

type PutAccountUpdateRole =
  ( "updateRole" :> Capture "personId" (Kernel.Types.Id.Id Dashboard.Common.Person) :> Capture "roleId" (Kernel.Types.Id.Id Dashboard.Common.Role)
      :> Put
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

newtype AccountAPIs = AccountAPIs {putAccountUpdateRole :: (Kernel.Types.Id.Id Dashboard.Common.Person -> Kernel.Types.Id.Id Dashboard.Common.Role -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}

mkAccountAPIs :: (Client EulerHS.Types.EulerClient API -> AccountAPIs)
mkAccountAPIs accountClient = (AccountAPIs {..})
  where
    putAccountUpdateRole = accountClient

data AccountUserActionType
  = PUT_ACCOUNT_UPDATE_ROLE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON AccountUserActionType where
  toJSON (PUT_ACCOUNT_UPDATE_ROLE) = Data.Aeson.String "PUT_ACCOUNT_UPDATE_ROLE"

instance FromJSON AccountUserActionType where
  parseJSON (Data.Aeson.String "PUT_ACCOUNT_UPDATE_ROLE") = pure PUT_ACCOUNT_UPDATE_ROLE
  parseJSON _ = fail "PUT_ACCOUNT_UPDATE_ROLE expected"

$(Data.Singletons.TH.genSingletons [(''AccountUserActionType)])
