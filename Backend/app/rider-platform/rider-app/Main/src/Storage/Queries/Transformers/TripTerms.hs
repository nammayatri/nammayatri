module Storage.Queries.Transformers.TripTerms where

import qualified Data.Text
import Kernel.Prelude
import Kernel.Utils.Common (MonadFlow, getCurrentTime)

-- descriptions on Tabular level is separated with '|' symbol
-- On Domain level it's list

intercalateDescriptions :: ([Data.Text.Text] -> Data.Text.Text)
intercalateDescriptions = Data.Text.intercalate "|"

splitDescriptions :: (Data.Text.Text -> [Data.Text.Text])
splitDescriptions = Data.Text.splitOn "|"

getCreatedAt :: MonadFlow m => Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> m Kernel.Prelude.UTCTime
getCreatedAt createdAt = do
  now <- getCurrentTime
  pure $ fromMaybe now createdAt

getUpdatedAt :: MonadFlow m => Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> m Kernel.Prelude.UTCTime
getUpdatedAt updatedAt = do
  now <- getCurrentTime
  pure $ fromMaybe now updatedAt
