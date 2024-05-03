{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DailyStats where

import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DailyStats = DailyStats
  { currency :: Kernel.Types.Common.Currency,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Data.Text.Text,
    merchantLocalDate :: Data.Time.Calendar.Day,
    numRides :: Kernel.Prelude.Int,
    totalDistance :: Kernel.Types.Common.Meters,
    totalEarnings :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)
