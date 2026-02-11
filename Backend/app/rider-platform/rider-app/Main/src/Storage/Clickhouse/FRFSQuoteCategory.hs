module Storage.Clickhouse.FRFSQuoteCategory where

import qualified Domain.Types.Quote as DQuote
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data FRFSQuoteCategoryT f = FRFSQuoteCategoryT
  { quoteId :: C f (Id DQuote.Quote),
    selectedQuantity :: C f (Maybe Int),
    createdAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show FRFSQuoteCategory

fRFSQuoteCategoryTTable :: FRFSQuoteCategoryT (FieldModification FRFSQuoteCategoryT)
fRFSQuoteCategoryTTable =
  FRFSQuoteCategoryT
    { quoteId = "quote_id",
      selectedQuantity = "selected_quantity",
      createdAt = "created_at"
    }

type FRFSQuoteCategory = FRFSQuoteCategoryT Identity

$(TH.mkClickhouseInstances ''FRFSQuoteCategoryT 'NO_SELECT_MODIFIER)

getQuoteCategoriesByQuoteIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DQuote.Quote] ->
  UTCTime ->
  UTCTime ->
  m [(Id DQuote.Quote, Maybe Int)]
getQuoteCategoriesByQuoteIds quoteIds startTime endTime = do
  CH.findAll $
    CH.select_ (\qc -> CH.notGrouped (qc.quoteId, qc.selectedQuantity)) $
      CH.selectModifierOverride CH.NO_SELECT_MODIFIER $
        CH.filter_
          ( \qc ->
              qc.quoteId `CH.in_` quoteIds
                CH.&&. qc.createdAt >=. startTime
                CH.&&. qc.createdAt <. endTime
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fRFSQuoteCategoryTTable)
