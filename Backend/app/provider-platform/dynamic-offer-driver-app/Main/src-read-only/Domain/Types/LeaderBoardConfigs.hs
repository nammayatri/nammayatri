{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.LeaderBoardConfigs where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data LeaderBoardConfigs = LeaderBoardConfigs
  { id :: Kernel.Types.Id.Id Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs,
    isEnabled :: Kernel.Prelude.Bool,
    leaderBoardExpiry :: Kernel.Types.Common.Seconds,
    leaderBoardLengthLimit :: Kernel.Prelude.Int,
    leaderBoardType :: Domain.Types.LeaderBoardConfigs.LeaderBoardType,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    numberOfSets :: Kernel.Prelude.Int,
    useOperatingCityBasedLeaderBoard :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    zScoreBase :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data LeaderBoardType = WEEKLY | DAILY | MONTHLY deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''LeaderBoardType)
