{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Dashboard.ProviderPlatform.CacAuth
  ( module Dashboard.ProviderPlatform.CacAuth,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Kernel.Prelude
import Servant hiding (Summary, throwError)

-- This is a temporary implementation of the CAC auth API. This will be depcricated once we have SSO for CAC.

data CacUser = CacUser
  { username :: String,
    email :: String,
    acl :: String,
    token :: String,
    auth_type :: String
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets CacUser where
  hideSecrets = identity

---------------------------- CAC --------------------------------
type CacAuthAPI =
  "auth"
    :> QueryParam "module" String
    :> QueryParam "access" String
    :> Header "Authorization" String
    :> Get '[JSON] CacUser
