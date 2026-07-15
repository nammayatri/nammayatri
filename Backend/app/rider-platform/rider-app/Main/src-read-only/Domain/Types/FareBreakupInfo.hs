{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FareBreakupInfo where

import Data.Aeson
import qualified Domain.Types.FareBreakup
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FareBreakupInfo = FareBreakupInfo
  { entityId :: Kernel.Prelude.Text,
    entityType :: Domain.Types.FareBreakup.FareBreakupEntityType,
    fareBreakups :: [Domain.Types.FareBreakupInfo.FareBreakupInfoItem],
    id :: Kernel.Types.Id.Id Domain.Types.FareBreakupInfo.FareBreakupInfo,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Show, (Generic), (ToJSON), (FromJSON), (ToSchema), (Eq))

data FareBreakupInfoItem = FareBreakupInfoItem {amount :: Kernel.Types.Common.HighPrecMoney, currency :: Kernel.Types.Common.Currency, description :: Kernel.Prelude.Text}
  deriving (Generic, (Show), (Eq), (FromJSON), (ToJSON), (ToSchema))
