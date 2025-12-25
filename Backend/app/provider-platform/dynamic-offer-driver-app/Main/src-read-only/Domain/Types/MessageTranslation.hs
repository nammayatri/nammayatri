{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MessageTranslation where

import Data.Aeson
import qualified Domain.Types.Message
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MessageTranslation = MessageTranslation
  { createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Text,
    label :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    language :: Kernel.External.Types.Language,
    messageId :: Kernel.Types.Id.Id Domain.Types.Message.Message,
    shortDescription :: Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
