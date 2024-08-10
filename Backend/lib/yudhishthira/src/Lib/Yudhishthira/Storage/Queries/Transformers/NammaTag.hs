{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.Transformers.NammaTag where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.NammaTag

getChakra :: (Lib.Yudhishthira.Types.NammaTag.TagInfo -> Kernel.Prelude.Maybe Lib.Yudhishthira.Types.Chakra)
getChakra tag =
  case tag of
    Lib.Yudhishthira.Types.NammaTag.Application (Lib.Yudhishthira.Types.NammaTag.ApplicationTagInfo _) -> Kernel.Prelude.Nothing
    Lib.Yudhishthira.Types.NammaTag.KaalChakra (Lib.Yudhishthira.Types.NammaTag.KaalChakraTagInfo chakra _) -> Just chakra

getEvent :: (Lib.Yudhishthira.Types.NammaTag.TagInfo -> Kernel.Prelude.Maybe Lib.Yudhishthira.Types.ApplicationEvent)
getEvent tag =
  case tag of
    Lib.Yudhishthira.Types.NammaTag.Application (Lib.Yudhishthira.Types.NammaTag.ApplicationTagInfo event) -> Just event
    Lib.Yudhishthira.Types.NammaTag.KaalChakra (Lib.Yudhishthira.Types.NammaTag.KaalChakraTagInfo _ _) -> Kernel.Prelude.Nothing

getTag :: (Lib.Yudhishthira.Types.NammaTag.TagInfo -> Lib.Yudhishthira.Types.NammaTag.TagType)
getTag tag =
  case tag of
    Lib.Yudhishthira.Types.NammaTag.Application _ -> Lib.Yudhishthira.Types.NammaTag.ApplicationTag
    Lib.Yudhishthira.Types.NammaTag.KaalChakra _ -> Lib.Yudhishthira.Types.NammaTag.KaalChakraTag

getValidity :: (Lib.Yudhishthira.Types.NammaTag.TagInfo -> Kernel.Prelude.Maybe Kernel.Types.Common.Hours)
getValidity tag =
  case tag of
    Lib.Yudhishthira.Types.NammaTag.Application (Lib.Yudhishthira.Types.NammaTag.ApplicationTagInfo _) -> Kernel.Prelude.Nothing
    Lib.Yudhishthira.Types.NammaTag.KaalChakra (Lib.Yudhishthira.Types.NammaTag.KaalChakraTagInfo _ validity) -> validity

mkTagInfo :: (Kernel.Prelude.Maybe Lib.Yudhishthira.Types.Chakra -> Kernel.Prelude.Maybe Lib.Yudhishthira.Types.ApplicationEvent -> Lib.Yudhishthira.Types.NammaTag.TagType -> Kernel.Prelude.Maybe Kernel.Types.Common.Hours -> Lib.Yudhishthira.Types.NammaTag.TagInfo)
mkTagInfo chakra event tagType validity =
  case tagType of
    Lib.Yudhishthira.Types.NammaTag.ApplicationTag -> Lib.Yudhishthira.Types.NammaTag.Application (Lib.Yudhishthira.Types.NammaTag.ApplicationTagInfo (fromMaybe Lib.Yudhishthira.Types.RideEnd event))
    Lib.Yudhishthira.Types.NammaTag.KaalChakraTag -> Lib.Yudhishthira.Types.NammaTag.KaalChakra (Lib.Yudhishthira.Types.NammaTag.KaalChakraTagInfo (fromMaybe Lib.Yudhishthira.Types.Monthly chakra) validity)

getRangeEnd :: (Lib.Yudhishthira.Types.TagValues -> Kernel.Prelude.Maybe Kernel.Prelude.Double)
getRangeEnd = \case
  Lib.Yudhishthira.Types.Range _ end -> Just end
  _ -> Kernel.Prelude.Nothing

getRangeStart :: (Lib.Yudhishthira.Types.TagValues -> Kernel.Prelude.Maybe Kernel.Prelude.Double)
getRangeStart = \case
  Lib.Yudhishthira.Types.Range start _ -> Just start
  _ -> Kernel.Prelude.Nothing

getTags :: (Lib.Yudhishthira.Types.TagValues -> Kernel.Prelude.Maybe [Kernel.Prelude.Text])
getTags = \case
  Lib.Yudhishthira.Types.Range _ _ -> Nothing
  Lib.Yudhishthira.Types.Tags tags -> Just tags

mkTagValues :: (Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe [Kernel.Prelude.Text] -> Lib.Yudhishthira.Types.TagValues)
mkTagValues rangeEnd rangeStart mbTags = case mbTags of
  Just tags -> Lib.Yudhishthira.Types.Tags tags
  Nothing -> Lib.Yudhishthira.Types.Range (fromMaybe 0 rangeStart) (fromMaybe 0 rangeEnd)
