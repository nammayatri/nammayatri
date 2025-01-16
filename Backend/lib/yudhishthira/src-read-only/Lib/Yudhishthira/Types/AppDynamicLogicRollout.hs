{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Types.AppDynamicLogicRollout where

import Data.Aeson
import qualified Data.Text
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data AppDynamicLogicRollout = AppDynamicLogicRollout
  { domain :: Lib.Yudhishthira.Types.LogicDomain,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Yudhishthira.Types.Merchant),
    merchantOperatingCityId :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity,
    percentageRollout :: Kernel.Prelude.Int,
    timeBounds :: Data.Text.Text,
    version :: Kernel.Prelude.Int,
    versionDescription :: Kernel.Prelude.Maybe Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
