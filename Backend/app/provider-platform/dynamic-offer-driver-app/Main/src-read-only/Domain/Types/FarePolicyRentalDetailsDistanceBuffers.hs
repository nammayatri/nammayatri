{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FarePolicyRentalDetailsDistanceBuffers where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Utils.Time
import qualified Tools.Beam.UtilsTH

data FarePolicyRentalDetailsDistanceBuffers = FarePolicyRentalDetailsDistanceBuffers {bufferKms :: Kernel.Prelude.Int, bufferMeters :: Kernel.Prelude.Int, farePolicyId :: Kernel.Prelude.Text, rideDuration :: Kernel.Utils.Time.Seconds}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
