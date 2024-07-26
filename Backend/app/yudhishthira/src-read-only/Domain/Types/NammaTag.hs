{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.NammaTag where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data NammaTag = NammaTag
  { category :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    info :: Domain.Types.NammaTag.TagInfo,
    name :: Kernel.Prelude.Text,
    possibleValues :: [Kernel.Prelude.Text],
    rule :: Lib.Yudhishthira.Types.TagRule,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype ApplicationTagInfo = ApplicationTagInfo {event :: Lib.Yudhishthira.Types.ApplicationEvent} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

data KaalChakraTagInfo = KaalChakraTagInfo {chakra :: Lib.Yudhishthira.Types.Chakra, validity :: Kernel.Prelude.Maybe Kernel.Types.Common.Hours}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

data TagInfo = Application Domain.Types.NammaTag.ApplicationTagInfo | KaalChakra Domain.Types.NammaTag.KaalChakraTagInfo deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data TagType = ApplicationTag | KaalChakraTag deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TagInfo))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TagType))
