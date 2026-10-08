{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSPassTicketStatistics where

import qualified Data.Time
import qualified Data.Time.Calendar
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSPassTicketStatisticsT f = FRFSPassTicketStatisticsT
  { createdAt :: (B.C f Data.Time.UTCTime),
    date :: (B.C f Data.Time.Calendar.Day),
    fareAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    purchasedPassPaymentId :: (B.C f Kernel.Prelude.Text),
    savedAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    ticketCount :: (B.C f Kernel.Prelude.Int),
    updatedAt :: (B.C f Data.Time.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSPassTicketStatisticsT where
  data PrimaryKey FRFSPassTicketStatisticsT f = FRFSPassTicketStatisticsId (B.C f Data.Time.Calendar.Day) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSPassTicketStatisticsId <$> date <*> purchasedPassPaymentId

type FRFSPassTicketStatistics = FRFSPassTicketStatisticsT Identity

$(enableKVPG (''FRFSPassTicketStatisticsT) [('date), ('purchasedPassPaymentId)] [])

$(mkTableInstances (''FRFSPassTicketStatisticsT) "frfs_pass_ticket_statistics")
