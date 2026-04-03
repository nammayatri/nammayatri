{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH



data FullFarePolicyProgressiveDetailsPerMinRateSection
    = FullFarePolicyProgressiveDetailsPerMinRateSection {currency :: Kernel.Types.Common.Currency,
                                                         farePolicyId :: Kernel.Prelude.Text,
                                                         perMinRate :: Kernel.Types.Common.HighPrecMoney,
                                                         rideDurationInMin :: Kernel.Prelude.Int,
                                                         createdAt :: Kernel.Prelude.UTCTime,
                                                         updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, Eq, ToJSON, FromJSON)



