{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverQuote.Internal where

import Domain.Types.DriverQuote as DriverQuote
import Domain.Types.Person as Person
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Queries.DriverQuote (baseDriverQuoteQuery)
import Storage.Queries.FullEntityBuilders (buildFullDriverQuote)
import Storage.Tabular.DriverQuote

getDriverQuote ::
  Transactionable m =>
  [Id Person] ->
  m [DriverQuote]
getDriverQuote personIds =
  buildDType $ do
    res <- Esq.findAll' $ do
      (dQuote :& farePars) <-
        from baseDriverQuoteQuery
      where_ $
        dQuote ^. DriverQuoteDriverId `in_` valList (toKey <$> personIds)
          &&. dQuote ^. DriverQuoteStatus ==. val DriverQuote.Active
      pure (dQuote, farePars)
    catMaybes <$> mapM buildFullDriverQuote res
