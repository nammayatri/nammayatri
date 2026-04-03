{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Lib.Yudhishthira.Storage.Queries.OrphanInstances.NammaTag where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Lib.Yudhishthira.Storage.Queries.Transformers.NammaTag
import qualified Lib.Yudhishthira.Types.NammaTag
import qualified Lib.Yudhishthira.Storage.Beam.NammaTag as Beam
import qualified Lib.Yudhishthira.Types



instance FromTType' Beam.NammaTag Lib.Yudhishthira.Types.NammaTag.NammaTag
    where fromTType' (Beam.NammaTagT {..}) = do pure $ Just Lib.Yudhishthira.Types.NammaTag.NammaTag{actionEngine = actionEngine,
                                                                                                     category = category,
                                                                                                     description = description,
                                                                                                     info = mkTagInfo chakra tagType,
                                                                                                     name = name,
                                                                                                     possibleValues = mkTagValues rangeEnd rangeStart tags,
                                                                                                     rule = mkTagRule llmContext ruleEngine,
                                                                                                     validity = validity,
                                                                                                     createdAt = createdAt,
                                                                                                     updatedAt = updatedAt}
instance ToTType' Beam.NammaTag Lib.Yudhishthira.Types.NammaTag.NammaTag
    where toTType' (Lib.Yudhishthira.Types.NammaTag.NammaTag {..}) = do Beam.NammaTagT{Beam.actionEngine = actionEngine,
                                                                                       Beam.category = category,
                                                                                       Beam.description = description,
                                                                                       Beam.chakra = getChakra info,
                                                                                       Beam.tagType = getTag info,
                                                                                       Beam.name = name,
                                                                                       Beam.rangeEnd = getRangeEnd possibleValues,
                                                                                       Beam.rangeStart = getRangeStart possibleValues,
                                                                                       Beam.tags = getTags possibleValues,
                                                                                       Beam.llmContext = getLlmContext rule,
                                                                                       Beam.ruleEngine = getRuleEngine rule,
                                                                                       Beam.validity = validity,
                                                                                       Beam.createdAt = createdAt,
                                                                                       Beam.updatedAt = updatedAt}



