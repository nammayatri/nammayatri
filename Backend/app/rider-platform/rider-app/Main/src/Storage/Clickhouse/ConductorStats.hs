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
    totalRevenueInADay :: C f Double,
    numberOfNewCustomers :: C f (Maybe Int),
    depotNo :: C f (Maybe Text)
  }
  deriving (Generic)

conductorStatsTTable :: ConductorStatsT (FieldModification ConductorStatsT)
conductorStatsTTable =
  ConductorStatsT
    { bookingDate = "booking_date",
      conductorTokenNo = "conductor_token_no",
      numberTicketsBooked = "number_tickets_booked",
      totalRevenueInADay = "total_revenue_in_a_day",
      numberOfNewCustomers = "number_of_new_customers",
      depotNo = "depot_no"
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

findConductorStatsBetween ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  Day ->
  Day ->
  m [ConductorStats]
findConductorStatsBetween token fromDay toDay =
  CH.findAll $
    CH.select $
      CH.filter_
        ( \row ->
            row.conductorTokenNo CH.==. token
              CH.&&. row.bookingDate CH.>=. fromDay
              CH.&&. row.bookingDate CH.<=. toDay
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE conductorStatsTTable)
