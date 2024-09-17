{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MultiModalCalendar where

import qualified Data.Time.Calendar
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MultiModalCalendar
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MultiModalCalendarT f = MultiModalCalendarT
  { endDate :: (B.C f Data.Time.Calendar.Day),
    friday :: (B.C f Domain.Types.MultiModalCalendar.ServiceType),
    id :: (B.C f Kernel.Prelude.Text),
    monday :: (B.C f Domain.Types.MultiModalCalendar.ServiceType),
    saturday :: (B.C f Domain.Types.MultiModalCalendar.ServiceType),
    startDate :: (B.C f Data.Time.Calendar.Day),
    sunday :: (B.C f Domain.Types.MultiModalCalendar.ServiceType),
    thursday :: (B.C f Domain.Types.MultiModalCalendar.ServiceType),
    tuesday :: (B.C f Domain.Types.MultiModalCalendar.ServiceType),
    wednesday :: (B.C f Domain.Types.MultiModalCalendar.ServiceType),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table MultiModalCalendarT where
  data PrimaryKey MultiModalCalendarT f = MultiModalCalendarId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MultiModalCalendarId . id

type MultiModalCalendar = MultiModalCalendarT Identity

$(enableKVPG (''MultiModalCalendarT) [('id)] [])

$(mkTableInstances (''MultiModalCalendarT) "multi_modal_calendar")
