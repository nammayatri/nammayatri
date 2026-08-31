{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverFyEarnings where

import Data.Aeson
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverFyEarnings = DriverFyEarnings
  { financialYear :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.DriverFyEarnings.DriverFyEarnings,
    netEarningsTotal :: Kernel.Types.Common.HighPrecMoney,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    quarter :: Kernel.Prelude.Int,
    tdsAmountTotal :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
