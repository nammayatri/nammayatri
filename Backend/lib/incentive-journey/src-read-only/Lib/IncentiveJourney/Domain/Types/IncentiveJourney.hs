{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Domain.Types.IncentiveJourney where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Tools.Beam.UtilsTH

data IncentiveJourney = IncentiveJourney
  { createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney,
    journeyType :: Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourneyType,
    maxWaiveOffCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data IncentiveJourneyType = Daily | Weekly | Monthly deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''IncentiveJourneyType)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''IncentiveJourneyType)
