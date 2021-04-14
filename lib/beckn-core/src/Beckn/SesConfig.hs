module Beckn.SesConfig (SesConfig (..), EmailRequestConfig (..)) where

import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude

newtype SesConfig = SesConfig
  { issuesConfig :: EmailRequestConfig
  }
  deriving (Generic, FromDhall)

data EmailRequestConfig = EmailRequestConfig
  { from :: Text,
    to :: [Text],
    replyTo :: [Text],
    cc :: [Text],
    region :: Text
  }
  deriving (Generic, FromDhall)
