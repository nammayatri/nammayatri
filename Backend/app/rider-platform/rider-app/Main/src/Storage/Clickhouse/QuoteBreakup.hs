{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Clickhouse.QuoteBreakup where

import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.QuoteBreakup as DQuoteBreakup
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id
import Kernel.Types.Price
import Kernel.Utils.Common

data QuoteBreakupT f = QuoteBreakupT
  { id :: C f (Id DQuoteBreakup.QuoteBreakup),
    priceValue :: C f Double,
    priceCurrency :: C f Currency,
    quoteId :: C f (Id DQuote.Quote),
    title :: C f Text,
    createdAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show QuoteBreakup

quoteBreakupTTable :: QuoteBreakupT (FieldModification QuoteBreakupT)
quoteBreakupTTable =
  QuoteBreakupT
    { id = "id",
      priceValue = "price_value",
      quoteId = "quote_id",
      priceCurrency = "price_currency",
      title = "title",
      createdAt = "created_at"
    }

type QuoteBreakup = QuoteBreakupT Identity

$(TH.mkClickhouseInstances ''QuoteBreakupT 'SELECT_FINAL_MODIFIER)

findAllByQuoteId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DQuote.Quote ->
  UTCTime ->
  m [QuoteBreakup]
findAllByQuoteId qId createdAt = do
  CH.findAll $
    CH.select $
      CH.filter_
        ( \quoteBreakup ->
            quoteBreakup.quoteId CH.==. qId
              CH.&&. quoteBreakup.createdAt >=. addUTCTime (-1800) createdAt -- quote expires in 30 mins
              CH.&&. quoteBreakup.createdAt <=. createdAt
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE quoteBreakupTTable)
