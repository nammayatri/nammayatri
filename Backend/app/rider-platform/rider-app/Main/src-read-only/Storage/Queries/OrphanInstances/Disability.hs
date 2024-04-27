{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Disability where

import qualified Domain.Types.Disability
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Disability as Beam

instance FromTType' Beam.Disability Domain.Types.Disability.Disability where
  fromTType' (Beam.DisabilityT {..}) = do pure $ Just Domain.Types.Disability.Disability {description = description, id = Kernel.Types.Id.Id id, tag = tag}

instance ToTType' Beam.Disability Domain.Types.Disability.Disability where
  toTType' (Domain.Types.Disability.Disability {..}) = do Beam.DisabilityT {Beam.description = description, Beam.id = Kernel.Types.Id.getId id, Beam.tag = tag}
