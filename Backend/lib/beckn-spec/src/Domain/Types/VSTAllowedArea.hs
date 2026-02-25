module Domain.Types.VSTAllowedArea
  ( VSTAllowedArea (..),
    vstAllowedAreaToText,
  )
where

import Data.Aeson
import Kernel.Prelude

newtype VSTAllowedArea = VSTAllowedArea Text
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

vstAllowedAreaToText :: VSTAllowedArea -> Text
vstAllowedAreaToText (VSTAllowedArea t) = t
