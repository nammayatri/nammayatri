{-# OPTIONS_GHC -Wwarn=unused-imports #-}

-- | A driver's own FY/quarter earnings.
--
-- The dashboard equivalent ('Domain.Action.Dashboard.Management.Driver.getDriverFyEarnings')
-- takes an @entityId@ and runs an association check. Here the driver is the
-- requestor, so the token already fixes whose rows these are -- there is nothing
-- to authorise beyond TokenAuth.
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
import qualified Storage.Queries.DriverFyEarnings as QDFE
import qualified Storage.Queries.FleetDriverAssociation as QFDA
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
  whenJust mbQuarter $ \q ->
    unless (q >= 1 && q <= 4) $
      throwError $ InvalidRequest "quarter must be between 1 and 4"
  -- A fleet driver's rides are attributed to the fleet owner, not to them: the
  -- accumulator keys on @fromMaybe ride.driverId ride.fleetOwnerId@, and
  -- ride.fleetOwnerId is taken from the vehicle's RC. Such a driver therefore has
  -- no rows at all, and a 200 with zeros would read as "you earned nothing"
  -- rather than "these earnings are not yours". Refuse instead.
  mbFleetAssoc <- QFDA.findByDriverId personId True
  whenJust mbFleetAssoc $ \_ -> throwError AccessDenied
  rows <- QDFE.findAllByPersonIdAndFinancialYear personId financialYear
  let wanted = maybe rows (\q -> filter (\r -> r.quarter == q) rows) mbQuarter
      quarters =
        map
          ( \r ->
              APIT.FyQuarterEarningsEntity
                { quarter = r.quarter,
                  netEarnings = r.netEarningsTotal,
                  tdsDeducted = r.tdsAmountTotal
                }
          )
          (sortOn (.quarter) wanted)
  pure
    APIT.DriverFyEarningsResp
      { financialYear = financialYear,
        quarters = quarters,
        totalNetEarnings = sum (map (.netEarnings) quarters),
        totalTdsDeducted = sum (map (.tdsDeducted) quarters)
      }
