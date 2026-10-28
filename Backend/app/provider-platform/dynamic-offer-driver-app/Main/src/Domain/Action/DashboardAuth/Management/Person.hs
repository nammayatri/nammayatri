{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handler for the merchant-scoped dashboard-person search.
--
-- The route lives in the direct-dashboard Management tree, so the merchant
-- short-id and operating city arrive as path captures (and the operator session
-- as 'DashboardUser'). The dashboard-database work is in
-- 'Tools.Auth.DashboardRegistration.listMerchantScopedPersons', which resolves
-- the requested merchant together with its logical pair and lists the persons
-- with access to either merchant in that city, all inside 'runInDashboardDb'.
--
-- Marked @appServerHandler: custom@ in Person.yaml because the data is
-- dashboard-local (atlas_dashboard), not a proxied driver-app call -- so the
-- generated @API.Action.DashboardAuth@ tree calls this function instead of a
-- generated domain handler, and this logic is never overwritten by the
-- generator.
module Domain.Action.DashboardAuth.Management.Person
  ( getPersonList,
  )
where

import qualified API.Types.ProviderPlatform.Management.Person
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import qualified Tools.Auth.DashboardRegistration
import Tools.Auth.DashboardUserAuth

getPersonList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.ProviderPlatform.Management.Person.ListPersonRes)
getPersonList merchantShortId city _dashboardUser mbSearchString mbLimit mbOffset =
  Tools.Auth.DashboardRegistration.listMerchantScopedPersons (Kernel.Types.Id.getShortId merchantShortId) city mbSearchString mbLimit mbOffset
