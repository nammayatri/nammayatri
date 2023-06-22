{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Payment.PaymentOrder where

import Domain.Types.Payment.PaymentOrder
import qualified Domain.Types.Payment.PaymentOrder as DOrder
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Juspay.Types as Payment
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Payment.PaymentOrder as BeamPO
import Storage.Tabular.Payment.PaymentOrder

findById :: Transactionable m => Id DOrder.PaymentOrder -> m (Maybe DOrder.PaymentOrder)
findById = Esq.findById

findByShortId :: Transactionable m => ShortId DOrder.PaymentOrder -> m (Maybe DOrder.PaymentOrder)
findByShortId shortId =
  findOne $ do
    order <- from $ table @PaymentOrderT
    where_ $ order ^. PaymentOrderShortId ==. val (getShortId shortId)
    return order

create :: L.MonadFlow m => DOrder.PaymentOrder -> m (MeshResult ())
create paymentOrder = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamPO.PaymentOrderT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainPaymentOrderToBeam paymentOrder)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

updateStatus :: DOrder.PaymentOrder -> SqlDB ()
updateStatus order = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PaymentOrderStatus =. val order.status,
        PaymentOrderUpdatedAt =. val now
      ]
    where_ $ tbl ^. PaymentOrderId ==. val order.id.getId

transformBeamPaymentOrderToDomain :: L.MonadFlow m => BeamPO.PaymentOrder -> m PaymentOrder
transformBeamPaymentOrderToDomain p@BeamPO.PaymentOrderT {..} = do
  paymentLinks <- parsePaymentLinks' p
  pure $
    PaymentOrder
      { id = Id id,
        shortId = ShortId shortId,
        customerId = Id customerId,
        merchantId = Id merchantId,
        amount = amount,
        currency = currency,
        status = status,
        paymentLinks = paymentLinks,
        clientAuthToken = EncryptedHashed (Encrypted clientAuthTokenEncrypted) clientAuthTokenHash,
        clientAuthTokenExpiry = clientAuthTokenExpiry,
        getUpiDeepLinksOption = getUpiDeepLinksOption,
        environment = environment,
        createdAt = createdAt,
        updatedAt = updatedAt
      }
  where
    parsePaymentLinks' obj = do
      web <- parseBaseUrl `mapM` obj.webPaymentLink
      iframe <- parseBaseUrl `mapM` obj.iframePaymentLink
      mobile <- parseBaseUrl `mapM` obj.mobilePaymentLink
      pure Payment.PaymentLinks {..}

transformDomainPaymentOrderToBeam :: PaymentOrder -> BeamPO.PaymentOrder
transformDomainPaymentOrderToBeam PaymentOrder {..} =
  BeamPO.defaultPaymentOrder
    { BeamPO.id = getId id,
      BeamPO.shortId = getShortId shortId,
      BeamPO.customerId = getId customerId,
      BeamPO.merchantId = getId merchantId,
      BeamPO.amount = amount,
      BeamPO.currency = currency,
      BeamPO.status = status,
      BeamPO.webPaymentLink = showBaseUrl <$> paymentLinks.web,
      BeamPO.iframePaymentLink = showBaseUrl <$> paymentLinks.iframe,
      BeamPO.mobilePaymentLink = showBaseUrl <$> paymentLinks.mobile,
      BeamPO.clientAuthTokenEncrypted = clientAuthToken & unEncrypted . (.encrypted),
      BeamPO.clientAuthTokenHash = clientAuthToken.hash,
      BeamPO.clientAuthTokenExpiry = clientAuthTokenExpiry,
      BeamPO.getUpiDeepLinksOption = getUpiDeepLinksOption,
      BeamPO.environment = environment,
      BeamPO.createdAt = createdAt,
      BeamPO.updatedAt = updatedAt
    }
