module Storage.Clickhouse.Quote where

import qualified Domain.Types.Quote as DQ
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id
import Kernel.Utils.Common

-- | Slim Clickhouse projection of the postgres @quote@ table — only the
-- columns we read on the breakup-fallback path. The @breakup_list_json@
-- column mirrors the new postgres column populated by
-- 'Storage.Queries.Transformers.Quote.encodeQuoteBreakupList'.
data QuoteT f = QuoteT
  { id :: C f (Id DQ.Quote),
    breakupListJson :: C f (Maybe Text),
    createdAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show Quote

quoteTTable :: QuoteT (FieldModification QuoteT)
quoteTTable =
  QuoteT
    { id = "id",
      breakupListJson = "quote_breakup_list_json",
      createdAt = "created_at"
    }

type Quote = QuoteT Identity

$(TH.mkClickhouseInstances ''QuoteT 'SELECT_FINAL_MODIFIER)

findById ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DQ.Quote ->
  UTCTime ->
  m (Maybe Quote)
findById quoteId createdAt = do
  res <-
    CH.findAll $
      CH.select $
        CH.filter_
          ( \quote ->
              quote.id CH.==. quoteId
                CH.&&. quote.createdAt >=. addUTCTime (-1800) createdAt
                CH.&&. quote.createdAt <=. createdAt
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE quoteTTable)
  pure (listToMaybe res)
