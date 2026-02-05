{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.Transformers.NammaTagV2 where

import qualified Data.Aeson
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.NammaTagV2

getChakra :: (Lib.Yudhishthira.Types.NammaTagV2.TagInfo -> Kernel.Prelude.Maybe Lib.Yudhishthira.Types.Chakra)
getChakra tag =
  case tag of
    Lib.Yudhishthira.Types.NammaTagV2.Application -> Kernel.Prelude.Nothing
    Lib.Yudhishthira.Types.NammaTagV2.KaalChakra (Lib.Yudhishthira.Types.NammaTagV2.KaalChakraTagInfo chakra) -> Just chakra
    Lib.Yudhishthira.Types.NammaTagV2.Manual -> Kernel.Prelude.Nothing

getTag :: (Lib.Yudhishthira.Types.NammaTagV2.TagInfo -> Lib.Yudhishthira.Types.NammaTagV2.TagType)
getTag tag =
  case tag of
    Lib.Yudhishthira.Types.NammaTagV2.Application -> Lib.Yudhishthira.Types.NammaTagV2.ApplicationTag
    Lib.Yudhishthira.Types.NammaTagV2.KaalChakra _ -> Lib.Yudhishthira.Types.NammaTagV2.KaalChakraTag
    Lib.Yudhishthira.Types.NammaTagV2.Manual -> Lib.Yudhishthira.Types.NammaTagV2.ManualTag

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
  Lib.Yudhishthira.Types.AnyText -> Nothing

getLlmContext :: (Lib.Yudhishthira.Types.TagRule -> Kernel.Prelude.Maybe Kernel.Prelude.Text)
getLlmContext = \case
  Lib.Yudhishthira.Types.RuleEngine _ -> Nothing
  Lib.Yudhishthira.Types.LLM llmContext -> Just llmContext

getRuleEngine :: (Lib.Yudhishthira.Types.TagRule -> Kernel.Prelude.Maybe Data.Aeson.Value)
getRuleEngine = \case
  Lib.Yudhishthira.Types.RuleEngine ruleEngine -> Just ruleEngine
  Lib.Yudhishthira.Types.LLM _ -> Nothing

mkTagInfo :: (Kernel.Prelude.Maybe Lib.Yudhishthira.Types.Chakra -> Lib.Yudhishthira.Types.NammaTagV2.TagType -> Lib.Yudhishthira.Types.NammaTagV2.TagInfo)
mkTagInfo chakra tagType =
  case tagType of
    Lib.Yudhishthira.Types.NammaTagV2.ApplicationTag -> Lib.Yudhishthira.Types.NammaTagV2.Application
    Lib.Yudhishthira.Types.NammaTagV2.KaalChakraTag -> Lib.Yudhishthira.Types.NammaTagV2.KaalChakra (Lib.Yudhishthira.Types.NammaTagV2.KaalChakraTagInfo (fromMaybe Lib.Yudhishthira.Types.Monthly chakra))
    Lib.Yudhishthira.Types.NammaTagV2.ManualTag -> Lib.Yudhishthira.Types.NammaTagV2.Manual

mkTagValues :: (Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe [Kernel.Prelude.Text] -> Lib.Yudhishthira.Types.TagValues)
mkTagValues rangeEnd rangeStart mbTags = case mbTags of
  Just tags -> Lib.Yudhishthira.Types.Tags tags
  Nothing -> case (rangeStart, rangeEnd) of
    (Just start, Just end) -> Lib.Yudhishthira.Types.Range start end
    _ -> Lib.Yudhishthira.Types.AnyText

mkTagRule :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Data.Aeson.Value -> Lib.Yudhishthira.Types.TagRule)
mkTagRule mbLlmContext mbRuleEngine = case (mbRuleEngine, mbLlmContext) of
  (Just ruleEngine, _) -> Lib.Yudhishthira.Types.RuleEngine ruleEngine
  (_, Just llmContext) -> Lib.Yudhishthira.Types.LLM llmContext
  (Nothing, Nothing) -> Lib.Yudhishthira.Types.RuleEngine Data.Aeson.Null
