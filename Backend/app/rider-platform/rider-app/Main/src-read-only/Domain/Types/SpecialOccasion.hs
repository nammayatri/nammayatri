{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.SpecialOccasion where

import qualified Data.Time.Calendar as Data.Time.Calendar
import qualified Domain.Types.BusinessHour as Domain.Types.BusinessHour
import Kernel.Prelude
import qualified Kernel.Types.Id as Kernel.Types.Id
import Tools.Beam.UtilsTH

data SpecialOccasion = SpecialOccasion
  { businessHours :: [Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour],
    date :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    dayOfWeek :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entityId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.SpecialOccasion.SpecialOccasion,
    specialDayType :: Domain.Types.SpecialOccasion.SpecialDayType
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SpecialDayType = Open | Closed
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''SpecialDayType)
