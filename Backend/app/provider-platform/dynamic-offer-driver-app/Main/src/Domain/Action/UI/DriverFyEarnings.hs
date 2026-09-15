{-# OPTIONS_GHC -Wwarn=unused-imports #-}

-- | A driver's own FY/quarter earnings.
--
-- The dashboard equivalent ('Domain.Action.Dashboard.Management.Driver.getDriverFyEarnings')
-- takes an @entityId@ and decides whether the requestor may read it. Here the
-- driver is the requestor, so the token already fixes whose rows these are and
-- there is nothing to authorise beyond TokenAuth.
--
-- A driver whose rides are attributed to a fleet owner simply has no rows, and
-- gets an empty result. That is deliberate: attribution is decided per ride
-- (@fromMaybe ride.driverId ride.fleetOwnerId@), so a driver who earned
-- independently before joining a fleet still sees those earlier quarters.
module Domain.Action.UI.DriverFyEarnings (getDriverFyEarnings) where

import qualified API.Types.UI.DriverFyEarnings as APIT
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverFyEarnings as SDFE
import Tools.Error

getDriverFyEarnings ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Int ->
    Environment.Flow APIT.DriverFyEarningsResp
  )
getDriverFyEarnings (mbPersonId, _, _) mbQuarter financialYear = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  SDFE.validateQuarter mbQuarter
  rows <- SDFE.getFyEarningsRows personId financialYear mbQuarter
  let quarters =
        map
          ( \r ->
              APIT.FyQuarterEarningsEntity
                { quarter = r.quarter,
                  netEarnings = r.netEarningsTotal,
                  tdsDeducted = r.tdsAmountTotal
                }
          )
          rows
  pure
    APIT.DriverFyEarningsResp
      { financialYear = financialYear,
        quarters = quarters,
        totalNetEarnings = sum (map (.netEarnings) quarters),
        totalTdsDeducted = sum (map (.tdsDeducted) quarters)
      }
