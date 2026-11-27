{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handler for the direct-dashboard ride info: the tier name and the feedback badges
-- are shown in the dashboard user's language.
module Domain.Action.DashboardAuth.Management.Ride
  ( getRideInfo,
  )
where

import qualified API.Types.ProviderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.Ride
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person.Type as Fields
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Tools.Auth.DashboardUser as Fields
import Tools.Auth.DashboardUserAuth

getRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Maybe [API.Types.ProviderPlatform.Management.Ride.RideDetailGroup] -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.Flow API.Types.ProviderPlatform.Management.Ride.RideInfoRes)
getRideInfo a6 a5 a4 a3 a2 a1 = Domain.Action.Dashboard.Management.Ride.getRideInfo a6 a5 a4.person.language a3 a2 a1
