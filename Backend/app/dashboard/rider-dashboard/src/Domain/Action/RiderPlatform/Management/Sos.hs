{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.Sos (getSosTracking) where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Sos
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getSosTracking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.Flow API.Types.RiderPlatform.Management.Sos.SosTrackingRes)
getSosTracking merchantShortId opCity sosId = do
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.sosDSL.getSosTracking) sosId
