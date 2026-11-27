{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.CancellationReasonLookup where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.TranslationKey
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data CancellationReasonResp = CancellationReasonResp {reasonCode :: Kernel.Prelude.Text, reasonMessage :: Domain.Types.TranslationKey.TranslatedText}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
