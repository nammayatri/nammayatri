{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup where

import Data.Aeson
import qualified Data.Text
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data AppDynamicLogicExperimentGroup = AppDynamicLogicExperimentGroup
  { experimentStatus :: Kernel.Prelude.Maybe Lib.Yudhishthira.Types.ExperimentStatus,
    groupName :: Data.Text.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Yudhishthira.Types.Merchant),
    merchantOperatingCityId :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity,
    modifiedBy :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Yudhishthira.Types.Person),
    rolloutPercentage :: Kernel.Prelude.Int,
    timeBounds :: Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
