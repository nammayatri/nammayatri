{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.BapMetadata where
import Kernel.Prelude
import Data.Aeson
import qualified Data.Text
import qualified Kernel.Types.Id
import qualified Servant.Client.Core
import qualified Tools.Beam.UtilsTH



data BapMetadata
    = BapMetadata {domain :: Kernel.Prelude.Maybe Data.Text.Text,
                   id :: Kernel.Types.Id.Id Domain.Types.BapMetadata.BapMetadata,
                   logoUrl :: Kernel.Prelude.Maybe Servant.Client.Core.BaseUrl,
                   name :: Data.Text.Text,
                   createdAt :: Kernel.Prelude.UTCTime,
                   updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



