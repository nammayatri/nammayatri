{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Insurance where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data InsuranceAPIEntity = InsuranceAPIEntity
  { certificateUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    message :: Kernel.Prelude.Text,
    plan :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    policyId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    policyNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
