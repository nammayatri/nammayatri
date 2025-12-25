{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.MerchantClientConfig where

import Kernel.External.Notification.FCM.Types as FT
import Kernel.Prelude
import qualified Text.Show
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data ClientService
  = ClientFCMService
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Show ClientService where
  show ClientFCMService = "Notification_FCM"

instance Read ClientService where
  readsPrec _ s
    | s == "Notification_FCM" = [(ClientFCMService, "")]
    | otherwise = []

$(mkBeamInstancesForEnum ''ClientService)

data ClientServiceConfig
  = ClientFCMServiceConfig FT.FCMConfig
  deriving (Generic, Eq, Show)

instance FromJSON ClientServiceConfig

instance ToJSON ClientServiceConfig
