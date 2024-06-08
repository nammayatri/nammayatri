{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.ReferalPayout where

import qualified API.Types.UI.ReferalPayout
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

-- getReferralEarnings ::
--   ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
--       Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
--       Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
--     ) ->
--     Kernel.Types.Id.Id Domain.Types.Person.Person ->
--     API.Types.UI.ReferalPayout.ReferralEarningsReq ->
--     Environment.Flow API.Types.UI.ReferalPayout.ReferralEarningsRes
--   )
-- getReferralEarnings (mbPersonId, merchantId, merchantOpCityId) personId req = Error

-- postDeleteVpa ::
--   ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
--       Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
--       Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
--     ) ->
--     Kernel.Types.Id.Id Domain.Types.Person.Person ->
--     Environment.Flow Kernel.Types.APISuccess.APISuccess
--   )
-- postDeleteVpa (mbPersonId, merchantId, merchantOpCityId) personId = do
--   -- QP.updatePayoutVpa
--   pure Success
