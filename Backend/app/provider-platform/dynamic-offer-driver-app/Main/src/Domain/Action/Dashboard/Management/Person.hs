{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.Person (getPersonList) where

import qualified API.Types.ProviderPlatform.Management.Person
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import Tools.Auth
import qualified Tools.Auth.DashboardRegistration

getPersonList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.ProviderPlatform.Management.Person.ListPersonRes)
getPersonList merchantShortId opCity searchString limit offset =
  Tools.Auth.DashboardRegistration.listMerchantScopedPersons (Kernel.Types.Id.getShortId merchantShortId) opCity searchString limit offset
