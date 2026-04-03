{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Lib.Yudhishthira.Storage.Queries.OrphanInstances.NammaTagV2 where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Lib.Yudhishthira.Storage.Queries.Transformers.NammaTagV2
import qualified Lib.Yudhishthira.Types.NammaTagV2
import qualified Lib.Yudhishthira.Storage.Beam.NammaTagV2 as Beam
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Types



instance FromTType' Beam.NammaTagV2 Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2
    where fromTType' (Beam.NammaTagV2T {..}) = do pure $ Just Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2{actionEngine = actionEngine,
                                                                                                           category = category,
                                                                                                           description = description,
                                                                                                           info = mkTagInfo chakra tagType,
                                                                                                           merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                           name = name,
                                                                                                           possibleValues = mkTagValues rangeEnd rangeStart tags,
                                                                                                           rule = mkTagRule llmContext ruleEngine,
                                                                                                           validity = validity,
                                                                                                           createdAt = createdAt,
                                                                                                           updatedAt = updatedAt}
instance ToTType' Beam.NammaTagV2 Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2
    where toTType' (Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2 {..}) = do Beam.NammaTagV2T{Beam.actionEngine = actionEngine,
                                                                                             Beam.category = category,
                                                                                             Beam.description = description,
                                                                                             Beam.chakra = getChakra info,
                                                                                             Beam.tagType = getTag info,
                                                                                             Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                             Beam.name = name,
                                                                                             Beam.rangeEnd = getRangeEnd possibleValues,
                                                                                             Beam.rangeStart = getRangeStart possibleValues,
                                                                                             Beam.tags = getTags possibleValues,
                                                                                             Beam.llmContext = getLlmContext rule,
                                                                                             Beam.ruleEngine = getRuleEngine rule,
                                                                                             Beam.validity = validity,
                                                                                             Beam.createdAt = createdAt,
                                                                                             Beam.updatedAt = updatedAt}



