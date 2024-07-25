module Lib.Yudhishthira.Types.Application where

import Kernel.Prelude
import Lib.Yudhishthira.Types.Common

data NammaTagApplication = NammaTagApplication
  { tagCategory :: Text,
    tagName :: Text,
    tagPossibleValues :: [Text],
    tagStage :: ApplicationEvent,
    tagRule :: TagRule
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data ApplicationEvent
  = Search
  | RideAssign
  | RideStart
  | RideEnd
  | RideCancel
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data ApplicationData = ApplicationData
  { ride :: Maybe Text,
    booking :: Maybe Text,
    search :: Maybe Text,
    riderDetails :: Maybe Text,
    driver :: Maybe Text,
    driverInfo :: Maybe Text
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)
