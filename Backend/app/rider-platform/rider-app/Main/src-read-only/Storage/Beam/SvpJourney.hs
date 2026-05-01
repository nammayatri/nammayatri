{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SvpJourney where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.SvpJourney
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data SvpJourneyT f = SvpJourneyT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    entryStationCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    entryTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    exitStationCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    exitTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    fareCharged :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    riderId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.SvpJourney.SvpJourneyStatus),
    tktSlNo :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table SvpJourneyT where
  data PrimaryKey SvpJourneyT f = SvpJourneyId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SvpJourneyId . id

type SvpJourney = SvpJourneyT Identity

$(enableKVPG (''SvpJourneyT) [('id)] [[('riderId)]])

$(mkTableInstances (''SvpJourneyT) "svp_journey")
