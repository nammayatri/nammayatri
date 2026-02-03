module Storage.Clickhouse.FRFSQuoteCategory where

import qualified Domain.Types.Quote as DQuote
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data FRFSQuoteCategoryT f = FRFSQuoteCategoryT
  { quoteId :: C f (Id DQuote.Quote),
    selectedQuantity :: C f (Maybe Int)
  }
  deriving (Generic)

deriving instance Show FRFSQuoteCategory

fRFSQuoteCategoryTTable :: FRFSQuoteCategoryT (FieldModification FRFSQuoteCategoryT)
fRFSQuoteCategoryTTable =
  FRFSQuoteCategoryT
    { quoteId = "quote_id",
      selectedQuantity = "selected_quantity"
    }

type FRFSQuoteCategory = FRFSQuoteCategoryT Identity

$(TH.mkClickhouseInstances ''FRFSQuoteCategoryT 'NO_SELECT_MODIFIER)

getQuoteCategoriesByQuoteIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DQuote.Quote] ->
  m [FRFSQuoteCategory]
getQuoteCategoriesByQuoteIds quoteIds = do
  CH.findAll $
    CH.select $
      CH.filter_
        (\qc -> qc.quoteId `CH.in_` quoteIds)
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fRFSQuoteCategoryTTable)
