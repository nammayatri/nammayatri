{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PersonStats where

import Data.Aeson
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PersonStats = PersonStats
  { personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    userCancelledRides :: Kernel.Prelude.Int,
    driverCancelledRides :: Kernel.Prelude.Int,
    completedRides :: Kernel.Prelude.Int,
    weekendRides :: Kernel.Prelude.Int,
    weekdayRides :: Kernel.Prelude.Int,
    offPeakRides :: Kernel.Prelude.Int,
    eveningPeakRides :: Kernel.Prelude.Int,
    morningPeakRides :: Kernel.Prelude.Int,
    weekendPeakRides :: Kernel.Prelude.Int,
    referralCount :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
