{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.MerchantPushNotification where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.External.Notification.Interface.Types
import qualified Kernel.Types.Id
import qualified Kernel.External.Types
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Trip
import qualified Tools.Beam.UtilsTH



data MerchantPushNotification
    = MerchantPushNotification {body :: Kernel.Prelude.Text,
                                fcmNotificationType :: Kernel.External.Notification.Interface.Types.Category,
                                fcmSubCategory :: Kernel.Prelude.Maybe Kernel.External.Notification.Interface.Types.SubCategory,
                                id :: Kernel.Types.Id.Id Domain.Types.MerchantPushNotification.MerchantPushNotification,
                                key :: Kernel.Prelude.Text,
                                language :: Kernel.External.Types.Language,
                                merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                                shouldTrigger :: Kernel.Prelude.Bool,
                                title :: Kernel.Prelude.Text,
                                tripCategory :: Kernel.Prelude.Maybe Domain.Types.Trip.TripCategory,
                                createdAt :: Kernel.Prelude.UTCTime,
                                updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, Eq)



