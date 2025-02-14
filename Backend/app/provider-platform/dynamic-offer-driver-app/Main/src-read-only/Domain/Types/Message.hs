{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Message where

import Data.Aeson
import qualified Data.Map
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified IssueManagement.Domain.Types.MediaFile
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Message = Message
  { _type :: Domain.Types.Message.MessageType,
    alwaysTriggerOnOnboarding :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Message.Message,
    label :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    likeCount :: Kernel.Prelude.Int,
    mediaFiles :: [Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile],
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    messageTranslations :: [Domain.Types.Message.MessageTranslation],
    shareable :: Kernel.Prelude.Bool,
    shortDescription :: Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text,
    viewCount :: Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data MessageDict = MessageDict {defaultMessage :: Domain.Types.Message.RawMessage, translations :: Data.Map.Map Kernel.Prelude.Text Domain.Types.Message.RawMessage}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data MessageTranslation = MessageTranslation
  { createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Text,
    label :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    language :: Kernel.External.Types.Language,
    shortDescription :: Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data MessageType = Action Kernel.Prelude.Text | Read deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RawMessage = RawMessage
  { _type :: Domain.Types.Message.MessageType,
    alwaysTriggerOnOnboarding :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Message.Message,
    label :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    likeCount :: Kernel.Prelude.Int,
    mediaFiles :: [Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile],
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    shareable :: Kernel.Prelude.Bool,
    shortDescription :: Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text,
    viewCount :: Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''MessageType)
