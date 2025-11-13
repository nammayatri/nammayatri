{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Clickhouse.ConductorStats where

import Data.Time.Calendar (Day)
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH

data ConductorStatsT f = ConductorStatsT
  { bookingDate :: C f Day,
    conductorTokenNo :: C f Text,
    numberTicketsBooked :: C f Int,
    totalRevenueInADay :: C f Double
  }
  deriving (Generic)

conductorStatsTTable :: ConductorStatsT (FieldModification ConductorStatsT)
conductorStatsTTable =
  ConductorStatsT
    { bookingDate = "booking_date",
      conductorTokenNo = "conductor_token_no",
      numberTicketsBooked = "number_tickets_booked",
      totalRevenueInADay = "total_revenue_in_a_day"
    }

type ConductorStats = ConductorStatsT Identity

deriving instance Show ConductorStats

$(TH.mkClickhouseInstances ''ConductorStatsT 'NO_SELECT_MODIFIER)

findConductorStatsByToken ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  m [ConductorStats]
findConductorStatsByToken token = do
  CH.findAll $
    CH.select $
      CH.filter_
        (\row -> row.conductorTokenNo CH.==. token)
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE conductorStatsTTable)
