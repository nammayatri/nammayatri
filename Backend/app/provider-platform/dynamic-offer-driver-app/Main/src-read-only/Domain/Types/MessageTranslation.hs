{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.MessageTranslation where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.External.Types
import qualified Kernel.Types.Id
import qualified Domain.Types.Message
import qualified Tools.Beam.UtilsTH



data MessageTranslation
    = MessageTranslation {createdAt :: Kernel.Prelude.UTCTime,
                          description :: Kernel.Prelude.Text,
                          label :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          language :: Kernel.External.Types.Language,
                          messageId :: Kernel.Types.Id.Id Domain.Types.Message.Message,
                          shortDescription :: Kernel.Prelude.Text,
                          title :: Kernel.Prelude.Text}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



