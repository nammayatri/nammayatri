{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SeatManagement where

import qualified Data.Time.Calendar
import qualified Database.Beam as B
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.SeatManagement
import qualified Domain.Types.ServiceCategory
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data SeatManagementT f = SeatManagementT
  { blocked :: B.C f Kernel.Prelude.Int,
    booked :: B.C f Kernel.Prelude.Int,
    date :: B.C f Data.Time.Calendar.Day,
    id :: B.C f Kernel.Prelude.Text,
    ticketServiceCategoryId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SeatManagementT where
  data PrimaryKey SeatManagementT f = SeatManagementId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = SeatManagementId . id

type SeatManagement = SeatManagementT Identity

$(enableKVPG ''SeatManagementT ['id] [])

$(mkTableInstances ''SeatManagementT "seat_management")
