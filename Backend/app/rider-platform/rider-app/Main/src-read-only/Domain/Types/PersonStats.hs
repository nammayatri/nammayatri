{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PersonStats where

import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PersonStats = PersonStats
  { completedRides :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    driverCancelledRides :: Kernel.Prelude.Int,
    eveningPeakRides :: Kernel.Prelude.Int,
    morningPeakRides :: Kernel.Prelude.Int,
    offPeakRides :: Kernel.Prelude.Int,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    updatedAt :: Kernel.Prelude.UTCTime,
    userCancelledRides :: Kernel.Prelude.Int,
    weekdayRides :: Kernel.Prelude.Int,
    weekendPeakRides :: Kernel.Prelude.Int,
    weekendRides :: Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
