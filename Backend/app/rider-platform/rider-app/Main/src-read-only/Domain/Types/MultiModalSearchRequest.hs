{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MultiModalSearchRequest where

import Data.Aeson
import qualified Domain.Types.Client
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data MultiModalSearchRequest = MultiModalSearchRequest
  { backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client),
    clientReactNativeVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    configInExperimentVersions :: [Lib.Yudhishthira.Types.ConfigVersionMap],
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest,
    isDashboardRequest :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    recentLocationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation),
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    startTime :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, (Show))
