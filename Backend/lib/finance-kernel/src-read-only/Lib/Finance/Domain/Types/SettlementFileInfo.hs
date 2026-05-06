{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.SettlementFileInfo where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data SettlementFileInfo = SettlementFileInfo
  { createdAt :: Kernel.Prelude.UTCTime,
    fileName :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo,
    lastProcessedIndex :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    paymentGatewayName :: Kernel.Prelude.Text,
    status :: Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data SettlementFileStatus = PENDING | IN_PROGRESS | COMPLETED | FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SettlementFileStatus))

$(mkHttpInstancesForEnum (''SettlementFileStatus))
