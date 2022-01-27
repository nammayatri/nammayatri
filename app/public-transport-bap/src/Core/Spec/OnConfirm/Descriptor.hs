module Core.Spec.OnConfirm.Descriptor where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Relude

newtype DescriptorCode = DescriptorCode
  { code :: Text
  }
  deriving (Generic, Show, Eq, ToSchema, ToJSON, FromJSON)
