{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.LeaderBoardConfigs where

import qualified Data.Text
import qualified Database.Beam as B
import qualified Domain.Types.LeaderBoardConfigs
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data LeaderBoardConfigsT f = LeaderBoardConfigsT
  { id :: B.C f Data.Text.Text,
    isEnabled :: B.C f Kernel.Prelude.Bool,
    leaderBoardExpiry :: B.C f Kernel.Types.Common.Seconds,
    leaderBoardLengthLimit :: B.C f Kernel.Prelude.Int,
    leaderBoardType :: B.C f Domain.Types.LeaderBoardConfigs.LeaderBoardType,
    merchantId :: B.C f Data.Text.Text,
    merchantOperatingCityId :: B.C f Data.Text.Text,
    numberOfSets :: B.C f Kernel.Prelude.Int,
    useOperatingCityBasedLeaderBoard :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    zScoreBase :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table LeaderBoardConfigsT where
  data PrimaryKey LeaderBoardConfigsT f = LeaderBoardConfigsId (B.C f Data.Text.Text)
    deriving (Generic, B.Beamable)
  primaryKey = LeaderBoardConfigsId . id

type LeaderBoardConfigs = LeaderBoardConfigsT Identity

$(enableKVPG ''LeaderBoardConfigsT ['id] [])

$(mkTableInstances ''LeaderBoardConfigsT "leader_board_configs")
