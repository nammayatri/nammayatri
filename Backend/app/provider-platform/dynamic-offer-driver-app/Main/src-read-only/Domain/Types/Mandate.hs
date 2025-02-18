{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Mandate where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data Mandate = Mandate
  { createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    endDate :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.Mandate.Mandate,
    mandatePaymentFlow :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    maxAmount :: Kernel.Types.Common.HighPrecMoney,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    payerApp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payerAppName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payerVpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startDate :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.Mandate.MandateStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data MandateStatus = ACTIVE | INACTIVE deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''MandateStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''MandateStatus)
