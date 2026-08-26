{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.GeohashArea where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data GeohashArea = GeohashArea
  { areaName :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    geohash :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.GeohashArea.GeohashArea,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, Read, FromJSON, ToJSON)
