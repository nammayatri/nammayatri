{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CorporateShift where

import Data.Aeson
import Data.Time (TimeOfDay)
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CorporateShift = CorporateShift
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift,
    corporateEntityId :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    pickupWindowStart :: Data.Time.TimeOfDay,
    pickupWindowEnd :: Data.Time.TimeOfDay,
    dropWindowStart :: Data.Time.TimeOfDay,
    dropWindowEnd :: Data.Time.TimeOfDay,
    activeDays :: Kernel.Prelude.Text,
    isNightShift :: Kernel.Prelude.Bool,
    maxOccupancy :: Kernel.Prelude.Int,
    allowedVehicleTiers :: Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
