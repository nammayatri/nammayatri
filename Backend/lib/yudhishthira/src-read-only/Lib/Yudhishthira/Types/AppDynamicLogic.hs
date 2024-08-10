{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Types.AppDynamicLogic where

import Data.Aeson
import qualified Data.Text
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data AppDynamicLogic = AppDynamicLogic
  { description :: Data.Text.Text,
    domain :: Data.Text.Text,
    logic :: Data.Aeson.Value,
    merchantOperatingCityId :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity,
    name :: Data.Text.Text,
    order :: Kernel.Prelude.Int,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
