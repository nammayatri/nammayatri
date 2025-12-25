module Storage.Queries.Transformers.WhiteListOrg where

import Kernel.Prelude
import Kernel.Utils.Common (MonadFlow, getCurrentTime)

getCreatedAt :: MonadFlow m => Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> m Kernel.Prelude.UTCTime
getCreatedAt createdAt = do
  now <- getCurrentTime
  pure $ fromMaybe now createdAt

getUpdatedAt :: MonadFlow m => Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> m Kernel.Prelude.UTCTime
getUpdatedAt updatedAt = do
  now <- getCurrentTime
  pure $ fromMaybe now updatedAt
