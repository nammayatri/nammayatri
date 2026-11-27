{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.CancellationReasons where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.TranslationKey
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

data CancellationReasonEntity = CancellationReasonEntity {code :: Data.Text.Text, iconUrl :: Data.Text.Text, text :: Domain.Types.TranslationKey.TranslatedText}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
