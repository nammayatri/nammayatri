{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.OneWayScheduledQuote where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)

getCreatedAt :: MonadFlow m => Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> m Kernel.Prelude.UTCTime
getCreatedAt createdAt = do
  now <- getCurrentTime
  pure $ fromMaybe now createdAt

getUpdatedAt :: MonadFlow m => Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> m Kernel.Prelude.UTCTime
getUpdatedAt updatedAt = do
  now <- getCurrentTime
  pure $ fromMaybe now updatedAt
