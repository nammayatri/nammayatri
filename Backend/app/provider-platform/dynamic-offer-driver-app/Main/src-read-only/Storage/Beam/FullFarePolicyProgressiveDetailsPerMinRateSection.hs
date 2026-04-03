{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FullFarePolicyProgressiveDetailsPerMinRateSection where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Database.Beam as B



data FullFarePolicyProgressiveDetailsPerMinRateSectionT f
    = FullFarePolicyProgressiveDetailsPerMinRateSectionT {currency :: (B.C f Kernel.Types.Common.Currency),
                                                          farePolicyId :: (B.C f Kernel.Prelude.Text),
                                                          perMinRate :: (B.C f Kernel.Types.Common.HighPrecMoney),
                                                          rideDurationInMin :: (B.C f Kernel.Prelude.Int),
                                                          createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                                          updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FullFarePolicyProgressiveDetailsPerMinRateSectionT
    where data PrimaryKey FullFarePolicyProgressiveDetailsPerMinRateSectionT f
              = FullFarePolicyProgressiveDetailsPerMinRateSectionId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Int)
              deriving (Generic, B.Beamable)
          primaryKey = FullFarePolicyProgressiveDetailsPerMinRateSectionId <$> farePolicyId <*> rideDurationInMin
type FullFarePolicyProgressiveDetailsPerMinRateSection = FullFarePolicyProgressiveDetailsPerMinRateSectionT Identity

$(enableKVPG (''FullFarePolicyProgressiveDetailsPerMinRateSectionT) [('farePolicyId), ('rideDurationInMin)] [])

$(mkTableInstances (''FullFarePolicyProgressiveDetailsPerMinRateSectionT) "fare_policy_progressive_details_per_min_rate_section")

