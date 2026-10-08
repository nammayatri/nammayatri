{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BapMetadata where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Servant.Client.Core
import qualified Tools.Beam.UtilsTH

data BapMetadata = BapMetadata
  { domain :: Data.Text.Text,
    enableOndcScheduledRideSupport :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.BapMetadata.BapMetadata,
    logoUrl :: Kernel.Prelude.Maybe Servant.Client.Core.BaseUrl,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    name :: Data.Text.Text,
    offlineContract :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    staticTermsUrl :: Kernel.Prelude.Maybe Servant.Client.Core.BaseUrl,
    supportEmail :: Kernel.Prelude.Maybe Data.Text.Text,
    supportPhone :: Kernel.Prelude.Maybe Data.Text.Text,
    supportUrl :: Kernel.Prelude.Maybe Servant.Client.Core.BaseUrl,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
