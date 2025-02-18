{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Notification where

import qualified "lib-dashboard" Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Notification = Notification
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.Notification.Notification,
    merchantShortId :: Kernel.Prelude.Text,
    metadata :: Kernel.Prelude.Text,
    notificationCategory :: Domain.Types.Notification.NotificationCategory,
    notificationCount :: Kernel.Prelude.Int,
    readStatus :: Kernel.Prelude.Bool,
    receiverId :: Kernel.Prelude.Text,
    senderId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data NotificationCategory
  = ADMIN_CHANGE_SUSPECT_STATUS
  | FLAG_REQUEST_UPLOAD
  | CHANGE_REQUEST_PARTNER_ADMIN
  | FLAG_REQUEST_APPROVED
  | PARTNER_FLAGGED_SUSPECT
  | FLAG_REQUEST_REJECTED
  | ADMIN_FLAGGED_SUSPECT
  | ADMIN_CHARGED_SUSPECT
  | ADMIN_CLEAN_SUSPECT
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''NotificationCategory)
