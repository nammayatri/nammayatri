{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module TransactionLogs.Interface
  ( pushTxnLogs,
  )
where

import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Servant.Client
import qualified TransactionLogs.Interface.ONDC as ONDC
import TransactionLogs.Interface.Types

pushTxnLogs ::
  (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) =>
  TransactionLogsConfigType ->
  TransactionLogReq ->
  m ()
pushTxnLogs logsConfig req = case logsConfig of
  ONDCCfg cfg -> ONDC.pushTxnLogs cfg req
