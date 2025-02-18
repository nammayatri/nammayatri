{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.OnSearchEvent where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data OnSearchEvent = OnSearchEvent
  { bppId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    errorType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.OnSearchEvent.OnSearchEvent,
    messageId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
