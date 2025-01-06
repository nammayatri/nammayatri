{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Toll where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.ComputeIntersection
import qualified Tools.Beam.UtilsTH

data Toll = Toll
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.Toll.Toll,
    isAutoRickshawAllowed :: Kernel.Prelude.Bool,
    isTwoWheelerAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    name :: Kernel.Prelude.Text,
    price :: Kernel.Types.Common.Price,
    tollEndGates :: [Kernel.Utils.ComputeIntersection.LineSegment],
    tollStartGates :: [Kernel.Utils.ComputeIntersection.LineSegment],
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, Read, FromJSON, ToJSON)
