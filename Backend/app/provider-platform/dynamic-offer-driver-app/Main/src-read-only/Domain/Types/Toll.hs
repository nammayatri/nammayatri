{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Toll where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.ComputeIntersection
import qualified Tools.Beam.UtilsTH

data Toll = Toll
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.Toll.Toll,
    name :: Kernel.Prelude.Text,
    price :: Kernel.Types.Common.Price,
    tollEndGates :: [Kernel.Utils.ComputeIntersection.LineSegment],
    tollStartGates :: [Kernel.Utils.ComputeIntersection.LineSegment],
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, Read, FromJSON, ToJSON)
