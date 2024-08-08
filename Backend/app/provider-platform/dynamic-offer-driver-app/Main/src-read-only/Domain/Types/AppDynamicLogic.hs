{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AppDynamicLogic where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TimeBound
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data AppDynamicLogic = AppDynamicLogic
  { description :: Data.Text.Text,
    domain :: Data.Text.Text,
    logic :: Data.Aeson.Value,
    name :: Data.Text.Text,
    order :: Kernel.Prelude.Int,
    timeBounds :: Domain.Types.TimeBound.TimeBound,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
