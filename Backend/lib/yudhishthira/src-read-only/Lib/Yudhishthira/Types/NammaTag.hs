{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Types.NammaTag where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data NammaTag = NammaTag
  { actionEngine :: Kernel.Prelude.Maybe Data.Aeson.Value,
    category :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    info :: Lib.Yudhishthira.Types.NammaTag.TagInfo,
    name :: Kernel.Prelude.Text,
    possibleValues :: Lib.Yudhishthira.Types.TagValues,
    rule :: Lib.Yudhishthira.Types.TagRule,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype ApplicationTagInfo = ApplicationTagInfo {event :: Lib.Yudhishthira.Types.ApplicationEvent} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

data KaalChakraTagInfo = KaalChakraTagInfo {chakra :: Lib.Yudhishthira.Types.Chakra, validity :: Kernel.Prelude.Maybe Kernel.Types.Common.Hours}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

data TagInfo
  = Application Lib.Yudhishthira.Types.NammaTag.ApplicationTagInfo
  | KaalChakra Lib.Yudhishthira.Types.NammaTag.KaalChakraTagInfo
  | Manual
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data TagType = ApplicationTag | KaalChakraTag | ManualTag deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''TagInfo)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''TagType)
