{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Actions the dashboard itself owns -- routes that proxy to servers other
-- than the two application servers (special-zone, bharat-taxi). They live here
-- rather than in either app because neither app serves them.
module Domain.Types.DashboardActionType (module Domain.Types.DashboardActionType, module Reexport) where

import Data.Singletons.TH
import qualified Data.Text as T
import Domain.Types.ServerName as Reexport (ServerName (..))
import qualified Domain.Types.ServerName as DSN
import Kernel.Prelude
import Tools.Auth.ApiAuth as Reexport (ApiAccessLevel (..), ApiEntity (..), ApiTokenInfo (..), IsUserActionType (..), type (/))
import qualified Tools.Auth.ApiAuth as Auth

data DashboardActionType
  = SPECIAL_ZONE_CREATE
  | SPECIAL_ZONE_DELETE
  | SPECIAL_ZONE_UPDATE
  | SPECIAL_ZONE_LOOKUP
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema, Eq, Ord)

instance Auth.IsUserActionType DashboardActionType where
  showUserActionType = T.pack . show

genSingletons [''DashboardActionType]

type ApiAuth (sn :: DSN.ServerName) (ae :: Auth.ApiEntity) (uat :: k) = Auth.ApiAuthFor DashboardActionType sn ae uat
