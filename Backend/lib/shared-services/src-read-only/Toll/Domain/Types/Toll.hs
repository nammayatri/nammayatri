{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Toll.Domain.Types.Toll where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.ComputeIntersection
import qualified Tools.Beam.UtilsTH

data Toll = Toll
  { id :: Kernel.Types.Id.Id Toll.Domain.Types.Toll.Toll,
    isAutoRickshawAllowed :: Kernel.Prelude.Bool,
    isTwoWheelerAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    name :: Kernel.Prelude.Text,
    price :: Kernel.Types.Common.Price,
    tollEndGates :: [Kernel.Utils.ComputeIntersection.LineSegment],
    tollStartGates :: [Kernel.Utils.ComputeIntersection.LineSegment],
    merchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Read, FromJSON, ToJSON)
