module Beckn.Types.Core.Migration.Page (Page (..)) where

import EulerHS.Prelude hiding (id)

data Page = Page
  { id :: Maybe Text,
    next_id :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
