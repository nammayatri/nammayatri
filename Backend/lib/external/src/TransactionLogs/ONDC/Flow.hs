{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module TransactionLogs.ONDC.Flow
  ( pushTxnLogsAPI,
  )
where

import EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (callAPI, fromEitherM)
import Servant hiding (throwError)
import TransactionLogs.ONDC.Types

type ONDCPushLogAPI =
  Header "Authorization" Text
    :> ReqBody '[JSON] ONDCRequest
    :> Post '[JSON] ONDCRequest

pushTxnLogsAPI ::
  (CoreMetrics m, MonadFlow m) =>
  ONDCConfig ->
  ONDCRequest ->
  m ONDCRequest
pushTxnLogsAPI config req = do
  withLogTag "Pushing txn logs to ONDC" $
    do
      let eulerClient = Euler.client (Proxy @ONDCPushLogAPI)
          npToken = config.apiToken
          url = config.url
      callAPI url (eulerClient (Just npToken) req) "pushTxnLogsAPI" (Proxy @ONDCPushLogAPI)
      >>= fromEitherM (\err -> InternalError $ "Failed to push txn logs: " <> show err)
