{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.NammaTag where

import qualified Domain.Types.NammaTag
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.NammaTag as Beam
import Storage.Queries.Transformers.NammaTag

instance FromTType' Beam.NammaTag Domain.Types.NammaTag.NammaTag where
  fromTType' (Beam.NammaTagT {..}) = do
    pure $
      Just
        Domain.Types.NammaTag.NammaTag
          { category = category,
            description = description,
            info = mkTagInfo chakra event tagType validity,
            name = name,
            possibleValues = mkTagValues rangeEnd rangeStart tags,
            rule = rule,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.NammaTag Domain.Types.NammaTag.NammaTag where
  toTType' (Domain.Types.NammaTag.NammaTag {..}) = do
    Beam.NammaTagT
      { Beam.category = category,
        Beam.description = description,
        Beam.chakra = getChakra info,
        Beam.event = getEvent info,
        Beam.tagType = getTag info,
        Beam.validity = getValidity info,
        Beam.name = name,
        Beam.rangeEnd = getRangeEnd possibleValues,
        Beam.rangeStart = getRangeStart possibleValues,
        Beam.tags = getTags possibleValues,
        Beam.rule = rule,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
