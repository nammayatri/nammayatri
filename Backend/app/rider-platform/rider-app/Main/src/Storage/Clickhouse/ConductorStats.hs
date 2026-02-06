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
    fleetNo :: C f (Maybe Text),
    depotNo :: C f (Maybe Text),
    numberTicketsBooked :: C f (Maybe Int),
    totalRevenueInADay :: C f (Maybe Double),
    numberOfNewCustomers :: C f (Maybe Int),
    date :: C f UTCTime
  }
  deriving (Generic)

conductorStatsTTable :: ConductorStatsT (FieldModification ConductorStatsT)
conductorStatsTTable =
  ConductorStatsT
    { bookingDate = "booking_date",
      conductorTokenNo = "conductor_token_no",
      fleetNo = "fleet_no",
      depotNo = "depot_no",
      numberTicketsBooked = "number_tickets_booked",
      totalRevenueInADay = "total_revenue_in_a_day",
      numberOfNewCustomers = "number_of_new_customers",
      date = "date"
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
