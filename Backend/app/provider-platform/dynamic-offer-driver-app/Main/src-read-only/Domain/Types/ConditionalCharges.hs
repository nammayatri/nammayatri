{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ConditionalCharges (module Domain.Types.ConditionalCharges, module ReExport) where

import Data.Aeson
import qualified Data.Text
import Domain.Types.Extra.ConditionalCharges as ReExport
import qualified Domain.Types.Extra.ConditionalCharges
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Utils.GenericPretty
import qualified Tools.Beam.UtilsTH

data ConditionalCharges = ConditionalCharges
  { cgstPercentage :: Kernel.Types.Common.HighPrecMoney,
    charge :: Kernel.Types.Common.HighPrecMoney,
    chargeCategory :: Domain.Types.Extra.ConditionalCharges.ConditionalChargesCategories,
    farePolicyId :: Data.Text.Text,
    sgstPercentage :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Ord, Eq, Read, Kernel.Utils.GenericPretty.PrettyShow)
