{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantConfigs where

import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantConfigs = MerchantConfigs
  { id :: Kernel.Types.Id.Id Domain.Types.MerchantConfigs.MerchantConfigs,
    merchantShortId :: Kernel.Prelude.Text,
    requestWebHook :: Kernel.Prelude.Bool,
    webHookHeaders :: [Domain.Types.MerchantConfigs.WebHookHeaders],
    webHookUrl :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data WebHookHeaders = WebHookHeaders {key :: Kernel.Prelude.Text, value :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Ord, Eq, Read)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''WebHookHeaders)
