module Lib.Yudhishthira.Storage.Queries.Transformers.NammaTag where

import qualified Data.Aeson
import Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.NammaTag

getChakra :: (Lib.Yudhishthira.Types.NammaTag.TagInfo -> Kernel.Prelude.Maybe Lib.Yudhishthira.Types.Chakra)
getChakra tag =
  case tag of
    Lib.Yudhishthira.Types.NammaTag.Application -> Kernel.Prelude.Nothing
    Lib.Yudhishthira.Types.NammaTag.KaalChakra (Lib.Yudhishthira.Types.NammaTag.KaalChakraTagInfo chakra) -> Just chakra
    Lib.Yudhishthira.Types.NammaTag.Manual -> Kernel.Prelude.Nothing

getTag :: (Lib.Yudhishthira.Types.NammaTag.TagInfo -> Lib.Yudhishthira.Types.NammaTag.TagType)
getTag tag =
  case tag of
    Lib.Yudhishthira.Types.NammaTag.Application -> Lib.Yudhishthira.Types.NammaTag.ApplicationTag
    Lib.Yudhishthira.Types.NammaTag.KaalChakra _ -> Lib.Yudhishthira.Types.NammaTag.KaalChakraTag
    Lib.Yudhishthira.Types.NammaTag.Manual -> Lib.Yudhishthira.Types.NammaTag.ManualTag

mkTagInfo :: (Kernel.Prelude.Maybe Lib.Yudhishthira.Types.Chakra -> Lib.Yudhishthira.Types.NammaTag.TagType -> Lib.Yudhishthira.Types.NammaTag.TagInfo)
mkTagInfo chakra tagType =
  case tagType of
    Lib.Yudhishthira.Types.NammaTag.ApplicationTag -> Lib.Yudhishthira.Types.NammaTag.Application
    Lib.Yudhishthira.Types.NammaTag.KaalChakraTag -> Lib.Yudhishthira.Types.NammaTag.KaalChakra (Lib.Yudhishthira.Types.NammaTag.KaalChakraTagInfo (fromMaybe Lib.Yudhishthira.Types.Monthly chakra))
    Lib.Yudhishthira.Types.NammaTag.ManualTag -> Lib.Yudhishthira.Types.NammaTag.Manual

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

getRuleEngine :: Lib.Yudhishthira.Types.TagRule -> Kernel.Prelude.Maybe Data.Aeson.Value
getRuleEngine = \case
  Lib.Yudhishthira.Types.RuleEngine ruleEngine -> Just ruleEngine
  Lib.Yudhishthira.Types.LLM _ -> Nothing

getLlmContext :: Lib.Yudhishthira.Types.TagRule -> Kernel.Prelude.Maybe Kernel.Prelude.Text
getLlmContext = \case
  Lib.Yudhishthira.Types.RuleEngine _ -> Nothing
  Lib.Yudhishthira.Types.LLM llmContext -> Just llmContext

mkTagValues :: (Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe [Kernel.Prelude.Text] -> Lib.Yudhishthira.Types.TagValues)
mkTagValues rangeEnd rangeStart mbTags = case mbTags of
  Just tags -> Lib.Yudhishthira.Types.Tags tags
  Nothing -> case (rangeStart, rangeEnd) of
    (Just start, Just end) -> Lib.Yudhishthira.Types.Range start end
    _ -> Lib.Yudhishthira.Types.AnyText

mkTagRule ::
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Data.Aeson.Value ->
  Lib.Yudhishthira.Types.TagRule
mkTagRule mbLlmContext mbRuleEngine = case (mbRuleEngine, mbLlmContext) of
  (Just ruleEngine, _) -> Lib.Yudhishthira.Types.RuleEngine ruleEngine
  (_, Just llmContext) -> Lib.Yudhishthira.Types.LLM llmContext
  (Nothing, Nothing) -> Lib.Yudhishthira.Types.RuleEngine Data.Aeson.Null
