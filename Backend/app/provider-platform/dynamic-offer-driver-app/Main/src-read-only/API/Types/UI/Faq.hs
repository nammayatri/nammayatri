{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Faq where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data FaqAPIEntity = FaqAPIEntity {answer :: Kernel.Prelude.Text, category :: Kernel.Prelude.Maybe Kernel.Prelude.Text, faqGroupId :: Kernel.Prelude.Text, question :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
