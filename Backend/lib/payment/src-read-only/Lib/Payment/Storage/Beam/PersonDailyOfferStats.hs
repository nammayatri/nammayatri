{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Payment.Storage.Beam.PersonDailyOfferStats where
import Kernel.Prelude
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Lib.Payment.Storage.Beam.BeamFlow ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Data.Time.Calendar
import qualified Lib.Payment.Domain.Types.Common
import qualified Database.Beam as B



data PersonDailyOfferStatsT f
    = PersonDailyOfferStatsT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                              currency :: (B.C f Kernel.Types.Common.Currency),
                              date :: (B.C f Data.Time.Calendar.Day),
                              id :: (B.C f Kernel.Prelude.Text),
                              merchantId :: (B.C f Kernel.Prelude.Text),
                              merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                              offerCount :: (B.C f Kernel.Prelude.Int),
                              payoutStatus :: (B.C f Lib.Payment.Domain.Types.Common.PayoutStatus),
                              personId :: (B.C f Kernel.Prelude.Text),
                              totalCashbackAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                              totalDiscountAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PersonDailyOfferStatsT
    where data PrimaryKey PersonDailyOfferStatsT f = PersonDailyOfferStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PersonDailyOfferStatsId . id
type PersonDailyOfferStats = PersonDailyOfferStatsT Identity

$(enableKVPG (''PersonDailyOfferStatsT) [('id)] [])

$(mkTableInstancesGenericSchema (''PersonDailyOfferStatsT) "person_daily_offer_stats")

