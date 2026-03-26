module Storage.Queries.FRFSQuoteCategoryExtra where

import qualified Data.List as DL
import Domain.Types.FRFSQuote
import Domain.Types.FRFSQuoteCategory
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSQuoteCategory as Beam
import Storage.Queries.OrphanInstances.FRFSQuoteCategory ()

sortQuoteCategoriesByCategoryOrder :: [FRFSQuoteCategory] -> [FRFSQuoteCategory]
sortQuoteCategoriesByCategoryOrder =
  DL.sortOn $ \qc ->
    case qc.categoryMeta >>= (.categoryOrder) of
      Just n -> Left n
      Nothing -> Right ()

findAllByQuoteId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id FRFSQuote ->
  m [FRFSQuoteCategory]
findAllByQuoteId quoteId = do
  quotes <- findAllWithKV [Se.Is Beam.quoteId $ Se.Eq (getId quoteId)]
  pure $ sortQuoteCategoriesByCategoryOrder quotes
