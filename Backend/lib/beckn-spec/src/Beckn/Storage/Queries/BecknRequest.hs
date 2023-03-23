{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Storage.Queries.BecknRequest where

import Beckn.Storage.Tabular.BecknRequest ()
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.BecknRequest
import Kernel.Utils.Common

logBecknRequest :: Text -> Text -> SqlDB ()
logBecknRequest reqJSON sign = do
  uuid <- generateGUID
  now <- getCurrentTime
  Esq.create $
    BecknRequest
      { id = uuid,
        timeStamp = now,
        becknRequest = reqJSON,
        signatureHeader = sign
      }
