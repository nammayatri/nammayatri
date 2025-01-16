module Lib.Yudhishthira.Types.Application where

import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Lib.Yudhishthira.Types.Common

data NammaTagApplication = NammaTagApplication
  { tagCategory :: Text,
    description :: Maybe Text,
    tagName :: Text,
    tagPossibleValues :: TagValues,
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
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''ApplicationEvent)
