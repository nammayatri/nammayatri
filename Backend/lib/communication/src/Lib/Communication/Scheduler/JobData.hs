module Lib.Communication.Scheduler.JobData where

import Kernel.Prelude
import Lib.Communication.Domain.Types.Communication (ChannelType)

data CommunicationDeliveryDispatchJobData = CommunicationDeliveryDispatchJobData
  { deliveryId :: Text,
    communicationId :: Text,
    channel :: ChannelType,
    recipientId :: Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    title :: Text,
    body :: Text,
    htmlBody :: Maybe Text,
    templateId :: Maybe Text,
    templateName :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)
