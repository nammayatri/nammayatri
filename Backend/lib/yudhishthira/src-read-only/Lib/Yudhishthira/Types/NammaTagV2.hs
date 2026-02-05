{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Types.NammaTagV2 where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data NammaTagV2 = NammaTagV2
  { actionEngine :: Kernel.Prelude.Maybe Data.Aeson.Value,
    category :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    info :: Lib.Yudhishthira.Types.NammaTagV2.TagInfo,
    merchantOperatingCityId :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    possibleValues :: Lib.Yudhishthira.Types.TagValues,
    rule :: Lib.Yudhishthira.Types.TagRule,
    validity :: Kernel.Prelude.Maybe Kernel.Types.Common.Hours,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype KaalChakraTagInfo = KaalChakraTagInfo {chakra :: Lib.Yudhishthira.Types.Chakra} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

data TagInfo = Application | KaalChakra Lib.Yudhishthira.Types.NammaTagV2.KaalChakraTagInfo | Manual deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data TagType = ApplicationTag | KaalChakraTag | ManualTag deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TagInfo))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TagType))
