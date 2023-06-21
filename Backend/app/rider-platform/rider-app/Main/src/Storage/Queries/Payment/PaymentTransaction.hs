{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Payment.PaymentTransaction where

import Domain.Types.Payment.PaymentTransaction as DTransaction
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import qualified Storage.Beam.Payment.PaymentTransaction as BeamPT
import Storage.Tabular.Payment.PaymentTransaction
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import qualified Sequelize as Se
import Lib.Utils

create :: L.MonadFlow m => PaymentTransaction -> m (MeshResult ())
create paymentTransaction = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamPT.PaymentTransactionT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainPaymentTransactionToBeam paymentTransaction)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

updateMultiple :: PaymentTransaction -> SqlDB ()
updateMultiple transaction = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PaymentTransactionStatusId =. val transaction.statusId,
        PaymentTransactionStatus =. val transaction.status,
        PaymentTransactionPaymentMethodType =. val transaction.paymentMethodType,
        PaymentTransactionPaymentMethod =. val transaction.paymentMethod,
        PaymentTransactionRespMessage =. val transaction.respMessage,
        PaymentTransactionRespCode =. val transaction.respCode,
        PaymentTransactionGatewayReferenceId =. val transaction.gatewayReferenceId,
        PaymentTransactionAmount =. val transaction.amount,
        PaymentTransactionCurrency =. val transaction.currency,
        PaymentTransactionJuspayResponse =. val transaction.juspayResponse,
        PaymentTransactionUpdatedAt =. val now
      ]
    where_ $ tbl ^. PaymentTransactionId ==. val transaction.id.getId

findByTxnUUID :: Transactionable m => Text -> m (Maybe PaymentTransaction)
findByTxnUUID txnUUID =
  findOne $ do
    transaction <- from $ table @PaymentTransactionT
    where_ $ transaction ^. PaymentTransactionTxnUUID ==. val txnUUID
    return transaction

transformBeamPaymentTransactionToDomain :: BeamPT.PaymentTransaction -> PaymentTransaction
transformBeamPaymentTransactionToDomain BeamPT.PaymentTransactionT {..} = do
  PaymentTransaction
    { id = Id id,
      txnUUID = txnUUID,
      paymentMethodType = paymentMethodType,
      paymentMethod = paymentMethod,
      respMessage = respMessage,
      respCode = respCode,
      gatewayReferenceId = gatewayReferenceId,
      orderId = Id orderId,
      merchantId = Id merchantId,
      amount = amount,
      currency = currency,
      dateCreated = dateCreated,
      statusId = statusId,
      status = status,
      juspayResponse = juspayResponse,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainPaymentTransactionToBeam :: PaymentTransaction -> BeamPT.PaymentTransaction
transformDomainPaymentTransactionToBeam PaymentTransaction {..} =
  BeamPT.defaultPaymentTransaction
    { BeamPT.id = getId id,
      BeamPT.txnUUID = txnUUID,
      BeamPT.paymentMethodType = paymentMethodType,
      BeamPT.paymentMethod = paymentMethod,
      BeamPT.respMessage = respMessage,
      BeamPT.respCode = respCode,
      BeamPT.gatewayReferenceId = gatewayReferenceId,
      BeamPT.orderId = getId orderId,
      BeamPT.merchantId = getId merchantId,
      BeamPT.amount = amount,
      BeamPT.currency = currency,
      BeamPT.dateCreated = dateCreated,
      BeamPT.statusId = statusId,
      BeamPT.status = status,
      BeamPT.juspayResponse = juspayResponse,
      BeamPT.createdAt = createdAt,
      BeamPT.updatedAt = updatedAt
    }
