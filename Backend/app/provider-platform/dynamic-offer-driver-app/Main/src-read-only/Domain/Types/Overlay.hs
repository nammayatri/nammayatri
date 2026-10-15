{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Overlay where

import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Plan
import qualified Domain.Types.VehicleCategory
import qualified Kernel.External.Notification.FCM.Types
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data OverlayD (s :: UsageSafety) = Overlay
  { actions :: [Kernel.Prelude.Text],
    actions2 :: [Kernel.External.Notification.FCM.Types.FCMActions],
    cancelButtonText :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    contactSupportNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    delay :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endPoint :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Overlay.Overlay,
    imageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    language :: Kernel.External.Types.Language,
    link :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    method :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    okButtonText :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    overlayKey :: Kernel.Prelude.Text,
    reqBody :: Data.Aeson.Value,
    secondaryActions :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    secondaryActions2 :: Kernel.Prelude.Maybe [Kernel.External.Notification.FCM.Types.FCMActions],
    showPushNotification :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    socialMediaLinks :: Kernel.Prelude.Maybe [Kernel.External.Notification.FCM.Types.FCMMediaLink],
    title :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toastMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    udf1 :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving (Generic, Show)

data OverlayCondition
  = PaymentOverdueGreaterThan Kernel.Prelude.Int
  | PaymentOverdueBetween Kernel.Prelude.Int Kernel.Prelude.Int
  | FreeTrialDaysLeft Kernel.Prelude.Int
  | InvoiceGenerated Domain.Types.Plan.PaymentMode
  | BlockedDrivers
  | InactiveAutopay
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

type Overlay = OverlayD 'Safe

instance FromJSON (OverlayD 'Unsafe)

instance ToJSON (OverlayD 'Unsafe)

instance FromJSON (OverlayD 'Safe)

instance ToJSON (OverlayD 'Safe)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''OverlayCondition)
