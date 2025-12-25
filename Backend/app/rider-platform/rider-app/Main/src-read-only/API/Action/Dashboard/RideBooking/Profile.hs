{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.Profile
  ( API.Types.Dashboard.RideBooking.Profile.API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Profile
import qualified Domain.Action.Dashboard.RideBooking.Profile
import qualified "this" Domain.Action.UI.Profile
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.Profile.API)
handler merchantId city = getProfileDetail merchantId city :<|> postProfileUpdate merchantId city

getProfileDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Domain.Action.UI.Profile.ProfileRes)
getProfileDetail a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Profile.getProfileDetail a3 a2 a1

postProfileUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Profile.UpdateProfileReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postProfileUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Profile.postProfileUpdate a4 a3 a2 a1
